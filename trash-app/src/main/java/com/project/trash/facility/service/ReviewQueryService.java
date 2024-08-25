package com.project.trash.facility.service;

import com.project.trash.common.exception.ValidationException;
import com.project.trash.facility.domain.Facility;
import com.project.trash.facility.domain.Review;
import com.project.trash.facility.repository.FacilityRepository;
import com.project.trash.facility.repository.ReviewRepository;
import com.project.trash.member.response.MyReviewListResponse;
import com.project.trash.utils.MemberUtils;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;

/**
 * 리뷰 조회 서비스
 */
@Service
@RequiredArgsConstructor
public class ReviewQueryService {

  private final ReviewRepository reviewRepository;
  private final FacilityRepository facilityRepository;

  /**
   * 등록한 리뷰 목록 조회
   */
  public List<MyReviewListResponse> getList() {
    // 1. 리뷰 목록 조회
    List<Review> reviews = reviewRepository.findAllByMemberSeq(MemberUtils.getMemberSeq());

    // 2. 시설물 ID 목록 추출
    List<String> facilityIds = reviews.stream().map(Review::getFacilityId).distinct().toList();

    // 3. 시설물 조회
    List<Facility> facilities = facilityRepository.findAllById(facilityIds);

    // 4. 시설물 ID와 시설물명 매핑
    Map<String, String> facilityMap =
        facilities.stream().collect(Collectors.toMap(Facility::getFacilityId, Facility::getName));

    return reviews.stream()
                  .map(
                      review -> new MyReviewListResponse(review.getReviewSeq(), facilityMap.get(review.getFacilityId()),
                          review.getContent(), review.getCreatedAt()))
                  .toList();
  }

  /**
   * 리뷰 조회
   */
  @Transactional(readOnly = true)
  public Review getOne(Long reviewSeq, Long memberSeq) {
    return reviewRepository.findByReviewSeqAndMemberSeq(reviewSeq, memberSeq)
                           .orElseThrow(() -> new ValidationException("review.not_found"));
  }
}
