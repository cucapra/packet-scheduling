"""Direct, timed controller-transaction file format shared by the CLIs."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Iterable

SCHEMA = "pifo-transactions-v1"
SUPPORTED_CONTROL_COMMANDS = {
    "UpdateMapperPre",
    "UpdateMapperPost",
    "UpdateMapperNonExist",
    "CommitMapper",
    "UpdateBrainEngine",
    "UpdateBrainState",
    "UpdateBrainFlowState",
}
HEADER_FIELDS = {
    "schema",
    "rootEngine",
    "rootVPifoId",
    "numEngines",
    "numVPifos",
    "maxPacketPriority",
    "fifoDepth",
    "prefetchBufferDepth",
}
TRANSACTION_FIELDS = {
    "at",
    "name",
    "mode",
    "before",
    "after",
    "drainRoot",
    "command",
    "engineId",
    "vPifoId",
    "flowId",
    "data",
}


@dataclass(frozen=True)
class ControllerCommand:
    command: str
    engine_id: int
    vpifo_id: int
    flow_id: int
    data: int

    def __post_init__(self) -> None:
        if self.command not in SUPPORTED_CONTROL_COMMANDS:
            raise ValueError(f"unsupported controller command {self.command!r}")
        if self.engine_id <= 0:
            raise ValueError("controller command engineId must be positive")
        if self.vpifo_id < 0 or self.flow_id < 0 or self.data < 0:
            raise ValueError("controller command numeric fields must be non-negative")


def controller_command_line(command: ControllerCommand) -> str:
    return (
        f"command={command.command} "
        f"engineId={command.engine_id} "
        f"vPifoId={command.vpifo_id} "
        f"flowId={command.flow_id} "
        f"data={command.data}"
    )


@dataclass(frozen=True)
class HardwareShape:
    num_engines: int
    num_vpifos: int
    max_packet_priority: int
    fifo_depth: int
    prefetch_buffer_depth: int

    def __post_init__(self) -> None:
        if self.num_engines <= 0:
            raise ValueError("numEngines must be positive")
        if self.num_vpifos < 3:
            raise ValueError("numVPifos must be at least 3")
        if self.max_packet_priority <= 1:
            raise ValueError("maxPacketPriority must be greater than one")
        if self.fifo_depth <= 0 or self.prefetch_buffer_depth <= 0:
            raise ValueError("FIFO and prefetch depths must be positive")
        capacity = self.num_vpifos * self.fifo_depth
        if capacity & (capacity - 1):
            raise ValueError("numVPifos * fifoDepth must be a power of two")


@dataclass(frozen=True)
class TimedTransaction:
    at_cycle: int | None
    name: str
    commands: tuple[ControllerCommand, ...]
    mode: str = "direct"
    before_label: str = ""
    after_label: str = ""
    drain_root: tuple[int, int] | None = None

    def __post_init__(self) -> None:
        if self.at_cycle is not None and self.at_cycle < 0:
            raise ValueError("transaction cycle must be non-negative")
        _require_token(self.name, "transaction name")
        if self.mode not in {"direct", "full_transitive"}:
            raise ValueError("transaction mode must be direct or full_transitive")
        if self.before_label:
            _require_token(self.before_label, "before label")
        if self.after_label:
            _require_token(self.after_label, "after label")
        if self.mode == "full_transitive" and self.drain_root is None:
            raise ValueError("full_transitive transaction requires drainRoot")
        if self.at_cycle is None and self.drain_root is not None:
            raise ValueError("initial transaction cannot have drainRoot")
        commit_indexes = [
            index
            for index, command in enumerate(self.commands)
            if command.command == "CommitMapper"
        ]
        if commit_indexes != [len(self.commands) - 1]:
            raise ValueError(
                f"transaction {self.name!r} must end with exactly one CommitMapper"
            )


@dataclass(frozen=True)
class TransactionProgram:
    hardware: HardwareShape
    root_engine_id: int
    root_vpifo_id: int
    initial: TimedTransaction | None
    transactions: tuple[TimedTransaction, ...]

    def __post_init__(self) -> None:
        if not 1 <= self.root_engine_id <= self.hardware.num_engines:
            raise ValueError("rootEngine is out of range")
        if not 0 <= self.root_vpifo_id < self.hardware.num_vpifos:
            raise ValueError("rootVPifoId is out of range")
        if self.initial is not None and self.initial.at_cycle is not None:
            raise ValueError("initial package must use at=init")
        if any(transaction.at_cycle is None for transaction in self.transactions):
            raise ValueError("timed transaction must have an integer cycle")
        names = [transaction.name for transaction in self.transactions]
        if len(set(names)) != len(names):
            raise ValueError("timed transaction names must be unique")
        cycles = [transaction.at_cycle for transaction in self.transactions]
        if cycles != sorted(cycles):
            raise ValueError("timed transactions must be ordered by cycle")
        max_flow_id = 1 << (
            self.hardware.num_engines.bit_length()
            + (self.hardware.num_vpifos - 1).bit_length()
        )
        for transaction in self._all_transactions():
            if transaction.drain_root is not None:
                drain_engine, drain_vpifo = transaction.drain_root
                if not 1 <= drain_engine <= self.hardware.num_engines:
                    raise ValueError(
                        f"transaction {transaction.name!r} drainRoot engine is out of range"
                    )
                if not 0 <= drain_vpifo < self.hardware.num_vpifos:
                    raise ValueError(
                        f"transaction {transaction.name!r} drainRoot vPifo is out of range"
                    )
            for command in transaction.commands:
                if command.engine_id > self.hardware.num_engines:
                    raise ValueError(
                        f"transaction {transaction.name!r} has engineId out of range"
                    )
                if command.vpifo_id >= self.hardware.num_vpifos:
                    raise ValueError(
                        f"transaction {transaction.name!r} has vPifoId out of range"
                    )
                if command.flow_id >= max_flow_id:
                    raise ValueError(
                        f"transaction {transaction.name!r} has flowId out of range"
                    )
                if command.data >= 2**32:
                    raise ValueError(
                        f"transaction {transaction.name!r} has data out of range"
                    )

    def _all_transactions(self) -> tuple[TimedTransaction, ...]:
        return ((self.initial,) if self.initial is not None else ()) + self.transactions


def write_transaction_program(path: Path, program: TransactionProgram) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    header = " ".join(
        (
            f"schema={SCHEMA}",
            f"rootEngine={program.root_engine_id}",
            f"rootVPifoId={program.root_vpifo_id}",
            f"numEngines={program.hardware.num_engines}",
            f"numVPifos={program.hardware.num_vpifos}",
            f"maxPacketPriority={program.hardware.max_packet_priority}",
            f"fifoDepth={program.hardware.fifo_depth}",
            f"prefetchBufferDepth={program.hardware.prefetch_buffer_depth}",
        )
    )
    lines = [header]
    for transaction in program._all_transactions():
        at = "init" if transaction.at_cycle is None else str(transaction.at_cycle)
        metadata = [
            f"at={at}",
            f"name={transaction.name}",
            f"mode={transaction.mode}",
        ]
        if transaction.before_label:
            metadata.append(f"before={transaction.before_label}")
        if transaction.after_label:
            metadata.append(f"after={transaction.after_label}")
        if transaction.drain_root is not None:
            metadata.append(
                f"drainRoot={transaction.drain_root[0]}:{transaction.drain_root[1]}"
            )
        prefix = " ".join(metadata)
        lines.extend(
            f"{prefix} {controller_command_line(command)}"
            for command in transaction.commands
        )
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def load_transaction_program(path: Path) -> TransactionProgram:
    records = list(_records(path))
    if not records:
        raise ValueError(f"{path}: empty transaction program")
    header_line, header = records[0]
    _exact_fields(path, header_line, header, HEADER_FIELDS)
    if header["schema"] != SCHEMA:
        raise ValueError(
            f"{path}:{header_line}: schema must be {SCHEMA!r}"
        )
    hardware = HardwareShape(
        num_engines=_integer(header["numEngines"], path, header_line),
        num_vpifos=_integer(header["numVPifos"], path, header_line),
        max_packet_priority=_integer(
            header["maxPacketPriority"], path, header_line
        ),
        fifo_depth=_integer(header["fifoDepth"], path, header_line),
        prefetch_buffer_depth=_integer(
            header["prefetchBufferDepth"], path, header_line
        ),
    )

    grouped: list[tuple[tuple[object, ...], list[ControllerCommand]]] = []
    seen_keys: set[tuple[str, str]] = set()
    for line_number, fields in records[1:]:
        unknown = set(fields).difference(TRANSACTION_FIELDS)
        if unknown:
            raise ValueError(
                f"{path}:{line_number}: unknown fields: {', '.join(sorted(unknown))}"
            )
        required = {
            "at",
            "name",
            "mode",
            "command",
            "engineId",
            "vPifoId",
            "flowId",
            "data",
        }
        missing = required.difference(fields)
        if missing:
            raise ValueError(
                f"{path}:{line_number}: missing fields: {', '.join(sorted(missing))}"
            )
        at_text = fields["at"]
        at_cycle = (
            None if at_text == "init" else _integer(at_text, path, line_number)
        )
        drain_root = (
            _parse_drain_root(fields["drainRoot"], path, line_number)
            if "drainRoot" in fields
            else None
        )
        metadata = (
            at_cycle,
            fields["name"],
            fields["mode"],
            fields.get("before", ""),
            fields.get("after", ""),
            drain_root,
        )
        group_key = (at_text, fields["name"])
        if not grouped or grouped[-1][0] != metadata:
            if group_key in seen_keys:
                raise ValueError(
                    f"{path}:{line_number}: transaction {fields['name']!r} is not contiguous"
                )
            grouped.append((metadata, []))
            seen_keys.add(group_key)
        grouped[-1][1].append(
            ControllerCommand(
                command=fields["command"],
                engine_id=_integer(fields["engineId"], path, line_number),
                vpifo_id=_integer(fields["vPifoId"], path, line_number),
                flow_id=_integer(fields["flowId"], path, line_number),
                data=_integer(fields["data"], path, line_number),
            )
        )

    transactions = tuple(
        TimedTransaction(
            at_cycle=metadata[0],
            name=metadata[1],
            mode=metadata[2],
            before_label=metadata[3],
            after_label=metadata[4],
            drain_root=metadata[5],
            commands=tuple(commands),
        )
        for metadata, commands in grouped
    )
    initial = tuple(item for item in transactions if item.at_cycle is None)
    if len(initial) > 1:
        raise ValueError(f"{path}: expected at most one at=init package")
    timed = tuple(item for item in transactions if item.at_cycle is not None)
    return TransactionProgram(
        hardware=hardware,
        root_engine_id=_integer(header["rootEngine"], path, header_line),
        root_vpifo_id=_integer(header["rootVPifoId"], path, header_line),
        initial=initial[0] if initial else None,
        transactions=timed,
    )


def _records(path: Path) -> Iterable[tuple[int, dict[str, str]]]:
    for line_number, raw in enumerate(path.read_text(encoding="utf-8").splitlines(), 1):
        text = raw.split("#", 1)[0].strip()
        if not text:
            continue
        fields: dict[str, str] = {}
        for token in text.split():
            key, separator, value = token.partition("=")
            if not separator or not key or not value:
                raise ValueError(
                    f"{path}:{line_number}: expected key=value token, got {token!r}"
                )
            if key in fields:
                raise ValueError(f"{path}:{line_number}: duplicate field {key!r}")
            fields[key] = value
        yield line_number, fields


def _exact_fields(
    path: Path,
    line_number: int,
    fields: dict[str, str],
    expected: set[str],
) -> None:
    missing = expected.difference(fields)
    unknown = set(fields).difference(expected)
    if missing:
        raise ValueError(
            f"{path}:{line_number}: missing fields: {', '.join(sorted(missing))}"
        )
    if unknown:
        raise ValueError(
            f"{path}:{line_number}: unknown fields: {', '.join(sorted(unknown))}"
        )


def _integer(value: str, path: Path, line_number: int) -> int:
    try:
        return int(value, 0)
    except ValueError as error:
        raise ValueError(
            f"{path}:{line_number}: invalid integer {value!r}"
        ) from error


def _parse_drain_root(
    value: str, path: Path, line_number: int
) -> tuple[int, int]:
    parts = value.split(":", 1)
    if len(parts) != 2:
        raise ValueError(f"{path}:{line_number}: drainRoot must be ENGINE:VPIFO")
    return (
        _integer(parts[0], path, line_number),
        _integer(parts[1], path, line_number),
    )


def _require_token(value: str, label: str) -> None:
    if not value or any(character.isspace() or character == "=" for character in value):
        raise ValueError(f"{label} must be a non-empty token without whitespace or '='")
