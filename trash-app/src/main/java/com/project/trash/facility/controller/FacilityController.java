package com.project.trash.facility.controller;

import com.project.trash.common.response.ListResponse;
import com.project.trash.common.response.SuccessResponse;
import com.project.trash.facility.controller.validation.FacilityValidator;
import com.project.trash.facility.request.FacilityEntryRequest;
import com.project.trash.facility.request.FacilityListRequest;
import com.project.trash.facility.request.FacilityModifyRequest;
import com.project.trash.facility.service.FacilityCommandService;
import com.project.trash.facility.service.FacilityQueryService;
import com.project.trash.review.request.ReviewEntryRequest;
import com.project.trash.review.request.ReviewModifyRequest;
import com.project.trash.review.service.ReviewCommandService;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

import lombok.RequiredArgsConstructor;

/**
 * 시설물 API
 */
@RestController
@RequestMapping("/facilities")
@RequiredArgsConstructor
public class FacilityController {

  private final FacilityCommandService facilityCommandService;
  private final FacilityQueryService facilityQueryService;

  private final ReviewCommandService reviewCommandService;

  /**
   * 시설물 삭제
   */
  @DeleteMapping("/{facilityId}")
  public ResponseEntity<?> delete(@PathVariable String facilityId) {
    facilityCommandService.delete(facilityId);
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
   * 시설물 목록 조회
   */
  @GetMapping
  public ResponseEntity<?> getList(@ModelAttribute FacilityListRequest param) {
    FacilityValidator.validate(param);

    return ResponseEntity.ok(new ListResponse(facilityQueryService.getList(param)));
  }

  /**
   * 시설물 등록
   */
  @PostMapping
  public ResponseEntity<?> post(@RequestPart FacilityEntryRequest param,
      @RequestPart(required = false) List<MultipartFile> images) {
    FacilityValidator.validate(param, images);

    facilityCommandService.entry(param, images);
    return ResponseEntity.ok(new SuccessResponse());
  }

  /**
   * 리뷰 등록
   */
  @PostMapping("/reviews")
  public ResponseEntity<?> postReview(@RequestBody ReviewEntryRequest param) {
    FacilityValidator.validate(param);

    reviewCommandService.entry(param);
    return ResponseEntity.ok(new SuccessResponse());
  }

  /**
   * 시설물 수정
   */
  @PutMapping
  public ResponseEntity<?> put(@RequestPart FacilityModifyRequest param,
      @RequestPart(required = false) List<MultipartFile> images) {
    FacilityValidator.validate(param, images);

    facilityCommandService.modify(param, images);
    return ResponseEntity.ok(new SuccessResponse());
  }

  /**
   * 리뷰 수정
   */
  @PutMapping("/reviews")
  public ResponseEntity<?> putReview(@RequestBody ReviewModifyRequest param) {
    FacilityValidator.validate(param);

    reviewCommandService.modify(param);
    return ResponseEntity.ok(new SuccessResponse());
  }
}
