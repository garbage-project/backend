package com.project.trash.member.controller;

import com.project.trash.common.response.DataResponse;
import com.project.trash.common.response.PageListResponse;
import com.project.trash.common.response.SuccessResponse;
import com.project.trash.facility.service.FacilityQueryService;
import com.project.trash.member.controller.validation.MemberValidator;
import com.project.trash.member.request.MemberFacilityListRequest;
import com.project.trash.member.request.MemberListRequest;
import com.project.trash.member.request.MemberReviewListRequest;
import com.project.trash.member.response.MemberFacilityListResponse;
import com.project.trash.member.response.MemberListResponse;
import com.project.trash.member.response.MemberReviewListResponse;
import com.project.trash.member.service.MemberCommandService;
import com.project.trash.member.service.MemberQueryService;
import com.project.trash.review.service.ReviewCommandService;
import com.project.trash.review.service.ReviewQueryService;

import org.apache.commons.lang3.tuple.Pair;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

import lombok.RequiredArgsConstructor;

/**
 * 회원 API
 */
@RestController
@RequestMapping("/members")
@RequiredArgsConstructor
public class MemberController {

  private final MemberQueryService memberQueryService;
  private final MemberCommandService memberCommandService;

  private final FacilityQueryService facilityQueryService;

  private final ReviewQueryService reviewQueryService;
  private final ReviewCommandService reviewCommandService;

  /**
   * 회원 삭제
   */
  @DeleteMapping("/{memberSeq}")
  public ResponseEntity<?> delete(@PathVariable Long memberSeq) {
    memberCommandService.delete(memberSeq);

    return ResponseEntity.ok(new SuccessResponse());
  }

  /**
   * 리뷰 삭제
   */
  @DeleteMapping("/reviews/{reviewSeq}")
  public ResponseEntity<?> deleteReview(@PathVariable Long reviewSeq) {
    reviewCommandService.delete(reviewSeq);

    return ResponseEntity.ok(new SuccessResponse());
  }

  /**
   * 회원 상세 조회
   */
  @GetMapping("/{memberSeq}")
  public ResponseEntity<?> getDetail(@PathVariable Long memberSeq) {
    return ResponseEntity.ok(new DataResponse(memberQueryService.getDetail(memberSeq)));
  }

  /**
   * 등록한 시설물 목록 조회
   */
  @GetMapping("/facilities")
  public ResponseEntity<?> getFacilityList(@ModelAttribute MemberFacilityListRequest param) {
    MemberValidator.validate(param);

    Pair<List<MemberFacilityListResponse>, Long> pair = facilityQueryService.getList(param);
    return ResponseEntity.ok(new PageListResponse(param, pair.getLeft(), pair.getRight()));
  }

  /**
   * 회원 목록 조회
   */
  @GetMapping
  public ResponseEntity<?> getList(@ModelAttribute MemberListRequest param) {
    MemberValidator.validate(param);

    Pair<List<MemberListResponse>, Long> pair = memberQueryService.getList(param);
    return ResponseEntity.ok(new PageListResponse(param, pair.getLeft(), pair.getRight()));
  }

  /**
   * 등록한 시설물 목록 조회
   */
  @GetMapping("/reviews")
  public ResponseEntity<?> getReviewList(@ModelAttribute MemberReviewListRequest param) {
    MemberValidator.validate(param);

    Pair<List<MemberReviewListResponse>, Long> pair = reviewQueryService.getList(param);
    return ResponseEntity.ok(new PageListResponse(param, pair.getLeft(), pair.getRight()));
  }
}
