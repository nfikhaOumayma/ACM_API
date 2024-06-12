/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.AddressSetting;

/**
 * Class provides service dao for {@link AddressSetting} table.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
@Repository
public interface AddressSettingRepository
		extends JpaRepository<AddressSetting, Long>, QuerydslPredicateExecutor<AddressSetting>,
		CrudRepository<AddressSetting, Long>, PagingAndSortingRepository<AddressSetting, Long> {

}
