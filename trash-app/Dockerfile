FROM openjdk:21

ENV TZ=Asia/Seoul
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

# copy jar
COPY trash-app/build/libs/*.jar app.jar

ENTRYPOINT ["java", "-Dspring.profiles.active=dev", "-jar", "app.jar"]