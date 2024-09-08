package com.project.trash.member.response;

import org.springframework.data.annotation.Id;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * 등록한 시설물 목록 응답
 */
@Getter
@Setter
@NoArgsConstructor
@Schema(title = "로그인 회원이 등록한 시설물 목록 조회 응답")
public class MyFacilityListResponse {

  /**
   * 시설물 ID
   */
  @Id
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
   * 상세 위치
   */
  @Schema(description = "상세 위치", example = "지하 1층")
  private String detailLocation;
  /**
   * 정보
   */
  @Schema(description = "정보", example = "개찰구 내에 존재합니다.", nullable = true)
  private String information;

  public MyFacilityListResponse(String facilityId, String type, String name, String detailLocation,
      String information) {
    this.facilityId = facilityId;
    this.type = type;
    this.name = name;
    this.detailLocation = detailLocation;
    this.information = information;
  }
}
