package com.acm.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.SettingChargeFee;

/**
 * Class provides Repo dao for {@link SettingChargeFee} table.
 *
 * @author meouertani
 * @since 0.1.0
 */
@Repository
public interface SettingChargeFeeRepository
		extends JpaRepository<SettingChargeFee, Long>, QuerydslPredicateExecutor<SettingChargeFee>,
		CrudRepository<SettingChargeFee, Long>, PagingAndSortingRepository<SettingChargeFee, Long> {

}
