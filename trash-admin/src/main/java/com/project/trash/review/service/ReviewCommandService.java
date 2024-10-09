package com.project.trash.review.service;

import com.project.trash.facility.domain.Review;
import com.project.trash.facility.repository.ReviewRepository;
import com.project.trash.member.request.MemberReviewModifyRequest;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Set;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
public class ReviewCommandService {

  private final ReviewRepository reviewRepository;
  private final ReviewQueryService reviewQueryService;

  @Transactional
  public void modify(MemberReviewModifyRequest param) {
    Review review = reviewQueryService.getOne(param.getReviewId());

    review.update(param.getContent());
  }

  @Transactional
  public void delete(Set<Long> reviewIds) {
    reviewRepository.deleteAllByIdInBatch(reviewIds);
  }
}
