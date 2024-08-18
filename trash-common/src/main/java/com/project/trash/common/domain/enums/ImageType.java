package com.project.trash.common.domain.enums;

import lombok.Getter;

/**
 * 이미지 종류
 */
@Getter
public enum ImageType {

  FACILITY("facility");

  private final String type;

  ImageType(String type) {
    this.type = type;
  }
}
