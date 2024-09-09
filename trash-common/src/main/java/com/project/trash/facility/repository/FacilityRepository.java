package com.project.trash.facility.repository;

import com.project.trash.facility.domain.Facility;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface FacilityRepository extends JpaRepository<Facility, Long> {

  Optional<Facility> findByFacilitySeqAndMemberId(Long facilitySeq, String memberId);
}
