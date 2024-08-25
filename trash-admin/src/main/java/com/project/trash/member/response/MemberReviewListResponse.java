package com.project.trash.member.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.project.trash.common.utils.DateTimeUtils;
import com.project.trash.facility.domain.Facility;

import lombok.Getter;
import lombok.Setter;
import trash.tables.records.ReviewRecord;

/**
 * 등록한 리뷰 목록 응답
 */
@Getter
@Setter
public class MemberReviewListResponse {

  /**
   * 리뷰 일련번호
   */
  private String reviewSeq;
  /**
   * 리뷰 내용
   */
  private String content;
  /**
   * 등록일자
   */
  private String createdDate;
  /**
   * 시설물 정보
   */
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
    private final String facilityId;
    /**
     * 시설물명
     */
    private final String name;
    /**
     * 시설물 종류
     */
    private final String type;
    /**
     * 위치
     */
    private final String location;

    public FacilityDetail(Facility facility) {
      this.facilityId = facility.getFacilityId();
      this.name = facility.getName();
      this.type = facility.getType().getCode();
      this.location = facility.getLocation();
    }
  }
}
