package com.project.trash.review.service;

import com.project.trash.facility.domain.Review;
import com.project.trash.facility.repository.ReviewRepository;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Set;

import lombok.RequiredArgsConstructor;

/**
 * 리뷰 등록/수정/삭제 서비스
 */
@Service
@RequiredArgsConstructor
public class ReviewCommandService {

  private final ReviewRepository reviewRepository;
  private final ReviewQueryService reviewQueryService;

  /**
   * 선택한 리뷰 목록 삭제
   */
  @Transactional
  public void delete(Set<Long> reviewIds) {
    reviewRepository.deleteAllByIdInBatch(reviewIds);
  }
}
