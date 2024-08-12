package com.project.trash.facility.repository;

import com.project.trash.facility.domain.Facility;

import org.springframework.data.mongodb.repository.MongoRepository;

public interface FacilityRepository extends MongoRepository<Facility, String> {
}
