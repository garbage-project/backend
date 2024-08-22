FROM openjdk:21

WORKDIR /jar

# copy jar
COPY build/libs/*.jar admin-server.jar

ENTRYPOINT ["java", "-Dspring.profiles.active=dev", "-jar", "admin-server.jar"]