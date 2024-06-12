package com.acm.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.PlaningStep;

@Repository
public interface PlaningStepRepository extends JpaRepository<PlaningStep, Long> {

}
