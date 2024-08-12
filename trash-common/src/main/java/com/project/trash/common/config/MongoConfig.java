package com.project.trash.common.config;

import com.project.trash.facility.domain.converter.FacilityApprovalStatusReadConverter;
import com.project.trash.facility.domain.converter.FacilityApprovalStatusWriteConverter;
import com.project.trash.facility.domain.converter.FacilityTypeReadConverter;
import com.project.trash.facility.domain.converter.FacilityTypeWriteConverter;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.mongodb.core.convert.MongoCustomConversions;

import java.util.Arrays;

@Configuration
public class MongoConfig {

  @Bean
  public MongoCustomConversions customConversions() {
    return new MongoCustomConversions(Arrays.asList(new FacilityTypeReadConverter(), new FacilityTypeWriteConverter(),
        new FacilityApprovalStatusReadConverter(), new FacilityApprovalStatusWriteConverter()));
  }
}
