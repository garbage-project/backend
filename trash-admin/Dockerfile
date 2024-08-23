FROM openjdk:21

ENV TZ=Asia/Seoul

# copy jar
COPY trash-admin/build/libs/*.jar app.jar

ENTRYPOINT ["java", "-Dspring.profiles.active=dev", "-jar", "app.jar"]