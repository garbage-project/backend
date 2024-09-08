package com.project.trash.member.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.project.trash.common.utils.DateTimeUtils;
import com.project.trash.facility.domain.Facility;

import org.jooq.types.ULong;

import java.time.LocalDateTime;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import trash.tables.records.FacilityRecord;
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
  private ULong reviewSeq;
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

  public MemberReviewListResponse(ULong reviewSeq, String content, LocalDateTime createdAt, FacilityRecord facility) {
    this.reviewSeq = reviewSeq;
    this.content = content;
    this.createdDate = DateTimeUtils.convertToString(createdAt, DateTimeUtils.DEFAULT_DATE);
    this.facility = new FacilityDetail(facility);
  }

  @Getter
  @Setter
  @JsonInclude(JsonInclude.Include.NON_NULL)
  public static class FacilityDetail {

    /**
     * 시설물 일련번호
     */
    @Schema(description = "시설물 일련번호", example = "1")
    private final ULong facilitySeq;
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

    public FacilityDetail(FacilityRecord facility) {
      this.facilitySeq = facility.getFcltySeq();
      this.type = facility.getFcltyTyp();
      this.name = facility.getFcltyNm();
      this.location = facility.getFcltyLctn();
    }
  }
}
