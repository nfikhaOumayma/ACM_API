/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;

import com.acm.utils.models.HabilitationIHMRoute;

/**
 * The {@link HabilitationIHMRouteRepository} Interface.
 *
 * @author HaythemBenizid
 * @since 0.9.0
 */
public interface HabilitationIHMRouteRepository extends JpaRepository<HabilitationIHMRoute, Long>,
		QuerydslPredicateExecutor<HabilitationIHMRoute>,
		CrudRepository<HabilitationIHMRoute, Long> {

}
