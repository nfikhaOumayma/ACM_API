package com.acm.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.Item;
import com.acm.utils.models.ItemRiskSetting;

/**
 * The Interface ItemRiskSettingRepository.
 */
@Repository
public interface ItemRiskSettingRepository extends JpaRepository<ItemRiskSetting, Long> {

	/**
	 * Find by item.
	 *
	 * @param item the item
	 * @return the list
	 */
	List<ItemRiskSetting> findByItem(Item item);

}
