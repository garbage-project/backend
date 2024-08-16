package com.project.trash.facility.request;

import java.util.Set;

import lombok.Getter;
import lombok.Setter;

/**
 * 시설물 목록 요청
 */
@Getter
@Setter
public class FacilityListRequest {

  /**
   * 시설물 종류
   */
  private Set<String> type;
}
