package com.project.trash.common.domain.resultcode;

import com.project.trash.common.domain.ResultCode;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum FacilityResultCode implements ResultCode {

  FACILITY_NOT_FOUND("FAC000", "시설물 정보가 존재하지 않습니다."),
  FACILITY_IMAGE_NOT_FOUND("FAC001", "시설물 이미지가 존재하지 않습니다."),
  FACILITY_EXCEL_EXTENSION_INVALID("FAC002", "시설물 데이터 엑셀 파일 확장자가 유효하지 않습니다."),
  FACILITY_EXCEL_READ_FAIL("FAC003", "엑셀 파일에서 시설물 데이터를 읽는데 실패했습니다.");

  private final String code;
  private final String message;
}
