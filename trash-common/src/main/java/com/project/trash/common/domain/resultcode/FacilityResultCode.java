package com.project.trash.common.domain.resultcode;

import com.project.trash.common.domain.ResultCode;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum FacilityResultCode implements ResultCode {

  FACILITY_NOT_FOUND("FAC000", "시설물 정보가 존재하지 않습니다.");

  private final String code;
  private final String message;
}
