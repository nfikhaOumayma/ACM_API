/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.query.Param;

import com.acm.utils.models.Habilitation;

/**
 * The {@link HabilitationRepository} Interface.
 *
 * @author HaythemBenizid
 * @since 0.3.0
 */
public interface HabilitationRepository extends JpaRepository<Habilitation, Long>,
		QuerydslPredicateExecutor<Habilitation>, CrudRepository<Habilitation, Long> {

	/**
	 * find habilitation by groupe code (only Enabled DATA).
	 *
	 * @author HaythemBenizid
	 * @param groupeId the groupe id
	 * @param enabled the enabled
	 * @return the list
	 */
	List<Habilitation> findByIdGroupeAndEnabled(Long groupeId, Boolean enabled);

	/**
	 * find habilitation by groupe ID.
	 *
	 * @author HaythemBenizid
	 * @param groupeId the groupe id
	 * @return the list
	 */
	List<Habilitation> findByIdGroupe(Long groupeId);

	/**
	 * Find by groupe id and client (only Enabled DATA).
	 *
	 * @author HaythemBenizid
	 * @param groupeId the groupe id
	 * @param client the client
	 * @param enabled the enabled
	 * @return the list
	 */
	List<Habilitation> findByIdGroupeAndClientAndEnabled(Long groupeId, String client,
			Boolean enabled);

	/**
	 * Find by groupe id and client and value (only Enabled DATA).
	 *
	 * @author HaythemBenizid
	 * @param groupeId the groupe id
	 * @param client the client
	 * @param value the value
	 * @param enabled the enabled
	 * @return the list
	 */
	List<Habilitation> findByIdGroupeAndClientAndValueAndEnabled(Long groupeId, String client,
			Boolean value, Boolean enabled);

	/**
	 * Find by id groupe and client and enabled and acm habilitation.
	 *
	 * @author kouali
	 * @param id the id
	 * @param appName the app name
	 * @param true1 the true 1
	 * @param codeHabilitationSetting the code habilitation setting
	 * @return the list
	 */
	List<Habilitation> findByIdGroupeAndClientAndEnabledAndAcmHabilitation(Long id, String appName,
			Boolean true1, String codeHabilitationSetting);

	/**
	 * Find by group id with habilitation.
	 *
	 * @author kouali
	 * @param groupId the group id
	 * @param client the client
	 * @param enabled the enabled
	 * @param lstModule the lst module
	 * @return the list
	 */
	@Query("SELECT habilitation FROM Habilitation habilitation  WHERE "
			+ "habilitation.idGroupe = :groupId and habilitation.client = :client and habilitation.enabled = :enabled "
			+ "and habilitation.habilitationIHMRoute.racineId.module in (:lstModule)  ")
	List<Habilitation> findByGroupIdWithHabilitation(@Param("groupId") Long groupId,
			@Param("client") String client, @Param("enabled") Boolean enabled,
			@Param("lstModule") List<String> lstModule);

	/**
	 * Find mac adress.
	 *
	 * @return the list
	 */
	@Query(value = "exec xp_cmdshell 'ipconfig /all'", nativeQuery = true)
	List<String> findMacAdress();

}
