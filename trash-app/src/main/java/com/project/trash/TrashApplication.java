package com.project.trash;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.ConfigurationPropertiesScan;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;

import java.util.TimeZone;

import jakarta.annotation.PostConstruct;

@EnableJpaAuditing
@ConfigurationPropertiesScan
@SpringBootApplication
public class TrashApplication {

  public static void main(String[] args) {
    SpringApplication.run(TrashApplication.class, args);
  }

  @PostConstruct
  public void init() {
    TimeZone.setDefault(TimeZone.getTimeZone("Asia/Seoul"));
  }
}
