package com.project.trash.facility.repository;

import com.project.trash.facility.domain.Facility;

import org.springframework.data.mongodb.repository.MongoRepository;

import java.util.Optional;

public interface FacilityRepository extends MongoRepository<Facility, String> {

  Optional<Facility> findByFacilityIdAndMemberSeq(String facilityId, Long memberSeq);
}
