package com.project.trash.facility.request;

import java.math.BigDecimal;
import java.util.List;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;

/**
 * 시설물 수정 요청
 */
@Getter
public class FacilityModifyRequest {

  /**
   * 시설물 ID
   */
  @Schema(description = "시설물 ID", example = "66c3194180a12933dd772938")
  private String facilityId;
  /**
   * 시설물 종류
   */
  @Schema(description = "시설물 종류 목록(R - 화장실, S - 흡연구역, T - 쓰레기통)", example = "R")
  private String type;
  /**
   * 시설물명
   */
  @Schema(description = "시설물명", example = "쌍문역 내 화장실")
  private String name;
  /**
   * 위치
   */
  @Schema(description = "위치", example = "쌍문역")
  private String location;
  /**
   * 상세 위치
   */
  @Schema(description = "상세 위치", example = "지하 1층")
  private String detailLocation;
  /**
   * 위도
   */
  @Schema(description = "위도", example = "2.6112336")
  private BigDecimal latitude;
  /**
   * 경도
   */
  @Schema(description = "경도", example = "33.126115")
  private BigDecimal longitude;
  /**
   * 정보
   */
  @Schema(description = "정보", example = "개찰구 내에 존재합니다.", nullable = true)
  private String information;
  /**
   * 이미지 인덱스 목록
   */
  private List<Integer> imageIndexes;
}
