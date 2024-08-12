package com.project.trash.facility.domain.enums;

import com.project.trash.common.domain.enums.Codable;

import java.util.EnumSet;

import lombok.Getter;

/**
 * 시설 타입
 */
@Getter
public enum FacilityType implements Codable {

  /**
   * 화장실
   */
  RESTROOM("R"),
  /**
   * 흡연구역
   */
  SMOKE("S"),
  /**
   * 쓰레기통
   */
  TRASH("T");

  private final String code;

  FacilityType(String code) {
    this.code = code;
  }

  public static boolean containCode(String code) {
    return EnumSet.allOf(FacilityType.class).stream().anyMatch(e -> e.getCode().equals(code));
  }

  public static FacilityType fromCode(String code) {
    return Codable.fromCode(FacilityType.class, code);
  }

}
