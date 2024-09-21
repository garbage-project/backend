package com.project.trash.facility.request;

import com.project.trash.common.request.PageRequest;

import java.math.BigDecimal;
import java.util.Set;

import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;

/**
 * 시설물 목록 요청
 */
@Getter
@Setter
public class FacilityListRequest extends PageRequest {

  @Parameter(description = "시설물 종류 목록(R - 화장실, S - 흡연구역, T - 쓰레기통)", example = "[\"R\", \"T\"]")
  private Set<String> type;

  @Parameter(description = "남쪽 위도", example = "37.413294", required = true)
  private BigDecimal southLat;

  @Parameter(description = "북쪽 위도", example = "37.701749", required = true)
  private BigDecimal northLat;

  @Parameter(description = "서쪽 경도", example = "126.764166", required = true)
  private BigDecimal westLng;

  @Parameter(description = "동쪽 경도", example = "127.181111", required = true)
  private BigDecimal eastLng;
}
