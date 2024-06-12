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
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.AcmDocuments;
import com.acm.utils.models.SettingDocumentType;

/**
 * Class provides service dao for {@link AcmDocuments} table.
 *
 * @author HaythemBenizid
 * @since 0.7.0
 */
@Repository
public interface AcmDocumentsRepository
		extends JpaRepository<AcmDocuments, Long>, QuerydslPredicateExecutor<AcmDocuments>,
		CrudRepository<AcmDocuments, Long>, PagingAndSortingRepository<AcmDocuments, Long> {

	/**
	 * Find documents by loan ID.
	 *
	 * @author HaythemBenizid
	 * @param loanId the loan id
	 * @param enabled the enabled
	 * @param settingDocumentTypes the setting document types
	 * @return the list
	 */
	List<AcmDocuments> findByLoanIdAndEnabledAndSettingDocumentTypeIn(Long loanId, Boolean enabled,
			List<SettingDocumentType> settingDocumentTypes);

	/**
	 * Find documents customer by telephone.
	 *
	 * @author hbeji
	 * @param telephone the telephone
	 * @return the list
	 */
	@Query(value = " select  d.* from ACM_CUSTOMER c  JOIN  ACM_DOCUMENTS d on  c.ID_ACM_CUSTOMER=d.ID_CUSTOMER where c.TELEPHONE_1=:telephone  and d.ACM_ENABLED=1",
			nativeQuery = true)
	List<AcmDocuments> findDocumentsCustomerByTelephone(String telephone);

	/**
	 * Find documents customer by telephone.
	 *
	 * @author hbeji
	 * @param collStepId the coll step id
	 * @param docTypeId the doc type id
	 * @return the list
	 */
	@Query(value = " SELECT REPORT_NAME FROM ACM_SETTING_DOC_PRODUCT dp JOIN ACM_COLLECTION c on dp.PRODUCT_ID =c.PRODUCT_ID JOIN ACM_COLLECTION_INSTANCE ci on ci.ID_ACM_COLLECTION =c.ID_ACM_COLLECTION WHERE ID_ACM_COLLECTION_INSTANCE = :collStepId AND ID_ACM_SETTING_DOC_TYPE = :docTypeId ",
			nativeQuery = true)
	List<String> getReportNameForCollectionStepDoc(Long collStepId, Long docTypeId);

	/**
	 * Find by setting document type and loan id.
	 *
	 * @author kouali
	 * @param settingDocumentType the setting document type
	 * @param loanId the loan id
	 * @param documentIndex the document index
	 * @return the list
	 */
	List<AcmDocuments> findBySettingDocumentTypeAndLoanIdAndDocumentIndex(
			SettingDocumentType settingDocumentType, long loanId, Integer documentIndex);

	/**
	 * Find by setting document type and id customer.
	 *
	 * @author kouali
	 * @param settingDocumentType the setting document type
	 * @param custumerId the custumer id
	 * @param documentIndex the document index
	 * @return the list
	 */
	List<AcmDocuments> findBySettingDocumentTypeAndIdCustomerAndDocumentIndexAndLoanIdIsNull(
			SettingDocumentType settingDocumentType, long custumerId, Integer documentIndex);

	/**
	 * Find by setting document type and collection instance id.
	 *
	 * @param settingDocumentType the setting document type
	 * @param collectionInstanceId the collection instance id
	 * @param documentIndex the document index
	 * @return the list
	 */
	List<AcmDocuments> findBySettingDocumentTypeAndCollectionInstanceIdAndDocumentIndex(
			SettingDocumentType settingDocumentType, long collectionInstanceId,
			Integer documentIndex);

	/**
	 * Find by setting document type and item instance id.
	 *
	 * @param settingDocumentType the setting document type
	 * @param itemInstanceStepId the item instance step id
	 * @return the list
	 */
	List<AcmDocuments> findBySettingDocumentTypeAndItemInstanceId(
			SettingDocumentType settingDocumentType, long itemInstanceStepId);

	/**
	 * Find by setting document type and element id and category.
	 *
	 * @param settingDocumentType the setting document type
	 * @param elementId the element id
	 * @param category the category
	 * @return the list
	 */
	List<AcmDocuments> findBySettingDocumentTypeAndElementIdAndCategory(
			SettingDocumentType settingDocumentType, long elementId, String category);
}
