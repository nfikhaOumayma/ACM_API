/**
 * Copyright(C)TALYS™-All Rights Reserved Unauthorized copying of this file,via any
 * medium/is*strictly prohibited Proprietary and confidential
 */
package com.acm.utils.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.AcmHistory;

/***
 * {@link AcmHistoryRepository} class.
 *
 * @author hchaouachi
 * @since 0.1.0
 */
@Repository
public interface AcmHistoryRepository extends JpaRepository<AcmHistory, Long> {

}
