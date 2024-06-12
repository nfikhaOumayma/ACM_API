/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.SimahClassType;

/**
 * The Interface SimahClassTypeRepository.
 */
@Repository
public interface SimahClassTypeRepository extends JpaRepository<SimahClassType, String> {

}
