package com.project.trash.facility.domain.enums;

import com.project.trash.common.domain.enums.Codable;

import java.util.EnumSet;

import lombok.Getter;

/**
 * 시설 승인 상태
 */
@Getter
public enum FacilityApprovalStatus implements Codable {

  /**
   * 승인 대기
   */
  PENDING("P"),
  /**
   * 승인 완료
   */
  APPROVE("A"),
  /**
   * 승인 거절
   */
  REJECT("R"),
  /**
   * 승인 중단
   */
  STOP("S");

  private final String code;

  FacilityApprovalStatus(String code) {
    this.code = code;
  }

  public static boolean containCode(String code) {
    return EnumSet.allOf(FacilityApprovalStatus.class).stream().anyMatch(e -> e.getCode().equals(code));
  }

  public static FacilityApprovalStatus fromCode(String code) {
    return Codable.fromCode(FacilityApprovalStatus.class, code);
  }

}
