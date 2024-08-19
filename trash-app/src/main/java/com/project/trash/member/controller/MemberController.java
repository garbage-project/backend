package com.project.trash.member.controller;

import com.project.trash.common.response.DataResponse;
import com.project.trash.common.response.ListResponse;
import com.project.trash.facility.service.FacilityQueryService;
import com.project.trash.member.service.MemberQueryService;
import com.project.trash.review.service.ReviewQueryService;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;

/**
 * 회원 API
 */
@RestController
@RequestMapping("/members")
@RequiredArgsConstructor
public class MemberController {

  private final MemberQueryService memberQueryService;
  private final FacilityQueryService facilityQueryService;
  private final ReviewQueryService reviewQueryService;

  /**
   * 등록한 시설물 목록 조회
   */
  @GetMapping("/my/facilities")
  public ResponseEntity<?> getMyFacilities() {
    return ResponseEntity.ok(new ListResponse(facilityQueryService.getList()));
  }

  /**
   * 로그인 회원 정보 조회
   */
  @GetMapping("/my")
  public ResponseEntity<?> getMyInfo() {
    return ResponseEntity.ok(new DataResponse(memberQueryService.getDetail()));
  }

  /**
   * 등록한 리뷰 목록 조회
   */
  @GetMapping("/my/reviews")
  public ResponseEntity<?> getMyReviews() {
    return ResponseEntity.ok(new ListResponse(reviewQueryService.getList()));
  }
}
