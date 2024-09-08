package com.project.trash.facility.domain.enums;

import com.project.trash.common.domain.converter.AbstractEnumCodeConverter;
import com.project.trash.common.domain.enums.Codable;

import java.util.EnumSet;

import jakarta.persistence.Converter;
import lombok.Getter;

/**
 * 시설물 종류
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

  @Converter
  public static class TypeCodeConverter extends AbstractEnumCodeConverter<FacilityType> {
    @Override
    public FacilityType convertToEntityAttribute(String dbData) {
      return this.toEntityAttribute(FacilityType.class, dbData);
    }
  }
}
