package com.project.trash.common.domain.converter;

import com.project.trash.common.domain.enums.Codable;

import org.apache.commons.lang3.StringUtils;

import jakarta.persistence.AttributeConverter;

/**
 * JPA Entity Enum Converter
 */
public abstract class AbstractEnumCodeConverter<E extends Enum<E> & Codable> implements AttributeConverter<E, String> {
  
  @Override
  public String convertToDatabaseColumn(E attribute) {
    return this.toDatabaseColumn(attribute);
  }

  @Override
  public E convertToEntityAttribute(String dbData) {
    return null;
  }

  public E toEntityAttribute(Class<E> cls, String code) {
    return StringUtils.isBlank(code) ? null : Codable.fromCode(cls, code);
  }

  private String toDatabaseColumn(E attr) {
    return (attr == null) ? null : attr.getCode();
  }
}
