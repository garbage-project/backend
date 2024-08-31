package com.project.trash.facility.repository;

import com.project.trash.facility.domain.Review;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface ReviewRepository extends JpaRepository<Review, Long> {

  List<Review> findAllByMemberSeq(Long memberSeq);

  Optional<Review> findByReviewSeqAndMemberSeq(Long reviewSeq, Long memberSeq);
}
