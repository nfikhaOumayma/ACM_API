/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import javax.transaction.Transactional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.EchelleTypeRisk;
import com.acm.utils.models.SettingTypeRisk;

/**
 * The Interface EchelleRiskTypeRepository.
 */
@Repository
public interface EchelleRiskTypeRepository extends JpaRepository<EchelleTypeRisk, Long> {

	/**
	 * Delete by setting type risk.
	 *
	 * @param settingTypeRiskUpdate the setting type risk update
	 */
	@Transactional
	void deleteBySettingTypeRisk(SettingTypeRisk settingTypeRiskUpdate);

}
