package com.project.trash;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;

@EnableJpaAuditing
@SpringBootApplication
public class TrashApplication {

  public static void main(String[] args) {
    SpringApplication.run(TrashApplication.class, args);
  }

}
