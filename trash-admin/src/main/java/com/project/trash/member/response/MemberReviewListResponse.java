package com.project.trash.member.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.project.trash.common.utils.DateTimeUtils;
import com.project.trash.facility.domain.Facility;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import trash.tables.records.ReviewRecord;

/**
 * 등록한 리뷰 목록 응답
 */
@Getter
@Setter
@Schema(title = "회원이 등록한 리뷰 목록 조회 응답")
public class MemberReviewListResponse {

  /**
   * 리뷰 일련번호
   */
  @Schema(description = "리뷰 일련번호", example = "1")
  private String reviewSeq;
  /**
   * 리뷰 내용
   */
  @Schema(description = "리뷰 내용", example = "시설물이 청결합니다~")
  private String content;
  /**
   * 등록일자
   */
  @Schema(description = "리뷰 등록일자", example = "2024-09-01")
  private String createdDate;
  /**
   * 시설물 정보
   */
  @Schema(description = "시설물 정보")
  private FacilityDetail facility;

  public MemberReviewListResponse(ReviewRecord review, Facility facility) {
    this.reviewSeq = String.valueOf(review.getRvwSeq());
    this.content = review.getRvwCtt();
    this.createdDate = DateTimeUtils.convertToString(review.getCreDtm(), DateTimeUtils.DEFAULT_DATE);
    this.facility = new FacilityDetail(facility);
  }

  @Getter
  @Setter
  @JsonInclude(JsonInclude.Include.NON_NULL)
  public static class FacilityDetail {

    /**
     * 시설물 ID
     */
    @Schema(description = "시설물 ID", example = "66c3194180a12933dd772938")
    private final String facilityId;
    /**
     * 시설물 종류
     */
    @Schema(description = "시설물 종류 목록(R - 화장실, S - 흡연구역, T - 쓰레기통)", example = "R")
    private final String type;
    /**
     * 시설물명
     */
    @Schema(description = "시설물명", example = "쌍문역 내 화장실")
    private final String name;
    /**
     * 위치
     */
    @Schema(description = "위치", example = "쌍문역")
    private final String location;

    public FacilityDetail(Facility facility) {
      this.facilityId = facility.getFacilityId();
      this.type = facility.getType().getCode();
      this.name = facility.getName();
      this.location = facility.getLocation();
    }
  }
}
