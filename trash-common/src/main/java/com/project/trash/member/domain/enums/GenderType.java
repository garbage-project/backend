package com.project.trash.member.domain.enums;

import com.project.trash.common.domain.converter.AbstractEnumCodeConverter;
import com.project.trash.common.domain.enums.Codable;

import java.util.EnumSet;

import jakarta.persistence.Converter;
import lombok.Getter;

/**
 * 회원 성별 타입
 */
@Getter
public enum GenderType implements Codable {
  /**
   * 남자
   */
  MALE("M", "남자"),
  /**
   * 여자
   */
  FEMALE("F", "여자"),
  /**
   * 확인불가
   */
  NONE("N", "확인불가");

  private final String code;
  private final String name;

  GenderType(String code, String name) {
    this.code = code;
    this.name = name;
  }

  public static boolean containCode(String code) {
    return EnumSet.allOf(GenderType.class).stream().anyMatch(e -> e.getCode().equals(code));
  }

  public static GenderType fromCode(String code) {
    return Codable.fromCode(GenderType.class, code);
  }

  @Converter
  public static class TypeCodeConverter extends AbstractEnumCodeConverter<GenderType> {
    @Override
    public GenderType convertToEntityAttribute(String dbData) {
      return this.toEntityAttribute(GenderType.class, dbData);
    }
  }
}
