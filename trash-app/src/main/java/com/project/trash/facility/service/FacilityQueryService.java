package com.project.trash.facility.service;

import com.project.trash.facility.repository.FacilityRepository;

import org.springframework.stereotype.Service;

import lombok.RequiredArgsConstructor;

/**
 * 시설물 조회 서비스
 */
@Service
@RequiredArgsConstructor
public class FacilityQueryService {

  private final FacilityRepository facilityRepository;
}
