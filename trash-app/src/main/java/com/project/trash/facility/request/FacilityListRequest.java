package com.project.trash.facility.request;

import java.math.BigDecimal;
import java.util.Set;

import io.swagger.v3.oas.annotations.Parameter;
import lombok.Getter;
import lombok.Setter;

/**
 * 시설물 목록 요청
 */
@Getter
@Setter
public class FacilityListRequest {

  @Parameter(description = "시설물 종류 목록(R - 화장실, S - 흡연구역, T - 쓰레기통)", example = "[\"R\", \"T\"]")
  private Set<String> type;

  @Parameter(description = "위도", example = "37.413294", required = true)
  private BigDecimal latitude;

  @Parameter(description = "경도", example = "126.764166", required = true)
  private BigDecimal longitude;
}
