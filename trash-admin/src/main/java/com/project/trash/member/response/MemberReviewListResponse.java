package com.project.trash.member.response;

import com.project.trash.common.utils.DateTimeUtils;

import org.jooq.types.ULong;

import java.time.LocalDateTime;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import trash.tables.records.FacilityRecord;

/**
 * 등록한 리뷰 목록 응답
 */
@Getter
@Setter
@Schema(title = "회원이 등록한 리뷰 목록 조회 응답")
public class MemberReviewListResponse {

  @Schema(description = "리뷰 ID", example = "1")
  private ULong reviewId;

  @Schema(description = "리뷰 내용", example = "시설물이 청결합니다~")
  private String content;

  @Schema(description = "리뷰 등록일자", example = "2024-09-01")
  private String createdDate;

  @Schema(description = "시설물 정보")
  private FacilityDetail facility;

  public MemberReviewListResponse(ULong reviewId, String content, LocalDateTime createdAt, FacilityRecord facility) {
    this.reviewId = reviewId;
    this.content = content;
    this.createdDate = DateTimeUtils.convertToString(createdAt, DateTimeUtils.DEFAULT_DATE);
    this.facility = new FacilityDetail(facility);
  }

  @Getter
  @Setter
  public static class FacilityDetail {

    @Schema(description = "시설물 ID", example = "1")
    private final ULong facilityId;

    @Schema(description = "시설물 종류 목록(R - 화장실, S - 흡연구역, T - 쓰레기통)", example = "R")
    private final String type;

    @Schema(description = "시설물명", example = "쌍문역 내 화장실")
    private final String name;

    @Schema(description = "위치", example = "쌍문역")
    private final String location;

    public FacilityDetail(FacilityRecord facility) {
      this.facilityId = facility.getFcltyId();
      this.type = facility.getFcltyTyp();
      this.name = facility.getFcltyNm();
      this.location = facility.getFcltyLctn();
    }
  }
}
