<template>
  <span v-bind="$attrs" class="tip" @click="openView">
    * 当前调拨单存在解冻操作，被解冻的物料数量已在调拨单中减少，点击可查看解冻的物料信息
  </span>
  <common-dialog
    title="解冻办理"
    v-model="dialogVisible"
    width="80%"
    :show-close="true"
    custom-class="wms-transfer-unfreeze-list"
    top="10vh"
  >
    <common-table
      ref="tableRef"
      :data="list"
      :max-height="maxHeight"
      :default-expand-all="false"
      :expand-row-keys="expandRowKeys"
      row-key="id"
    >
      <el-expand-table-column :data="list" v-model:expand-row-keys="expandRowKeys" row-key="id">
        <template #default="{ row }">
          <expand-secondary-info v-if="row.material" :basic-class="row.material.basicClass" :row="row.material">
            <p>
              备注：<span v-empty-text>{{ row.remark }}</span>
            </p>
          </expand-secondary-info>
        </template>
      </el-expand-table-column>
      <!-- 基础信息 -->
      <material-base-info-columns :basic-class="basicClass" spec-merge />
      <!-- 单位及其数量 -->
      <material-unit-quantity-columns :basic-class="basicClass" label-prefix="解冻" outbound-type-mode />
      <!-- 次要信息 -->
      <material-secondary-info-columns :basic-class="basicClass" :show-batch-no="false" />
      <warehouse-info-columns show-project />
      <el-table-column key="founderName" prop="founderName" label="解冻人" align="center" width="90" show-overflow-tooltip />
      <el-table-column key="createTime" prop="createTime" label="解冻时间" align="center" width="140" show-overflow-tooltip>
        <template #default="{ row: record }">
          <span v-parse-time="record.createTime" />
        </template>
      </el-table-column>
    </common-table>
  </common-dialog>
</template>

<script setup>
import { ref, defineProps, watch } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import ElExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import ExpandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'
import MaterialBaseInfoColumns from '@/components-system/wms/table-custom-field-columns/material-base-info-columns/index.vue'
import MaterialUnitQuantityColumns from '@/components-system/wms/table-custom-field-columns/material-unit-quantity-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-custom-field-columns/material-secondary-info-columns/index.vue'
import WarehouseInfoColumns from '@/components-system/wms/table-custom-field-columns/warehouse-info-columns/index.vue'
import { deepClone, isBlank } from '@/utils/data-type'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'

const props = defineProps({
  basicClass: {
    type: Number
  },
  list: {
    type: Array,
    default: () => []
  }
})

const dialogVisible = ref()
const expandRowKeys = ref()
// 解冻记录列表
const list = ref([])

watch(
  () => props.list,
  (val) => {
    setList(val)
  },
  { immediate: true }
)

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.wms-transfer-unfreeze-list',
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true
  },
  dialogVisible
)

// 设置列表
async function setList(unfreezeList) {
  list.value = []
  if (isBlank(unfreezeList)) return
  let materialList = []
  const _list = deepClone(unfreezeList)
  _list.forEach((row) => materialList.push(row.material))
  await setSpecInfoToList(materialList)
  materialList = await numFmtByBasicClass(materialList, {
    toSmallest: false,
    toNum: false
  })
  _list.forEach((row, index) => {
    row.material = materialList[index]
  })
  list.value = _list
}

// 查看信息
function openView() {
  dialogVisible.value = true
}
</script>

<style lang="scss" scoped>
.tip {
  display: inline-block;
  cursor: pointer;
  color: red;
  text-decoration: underline;
  margin-bottom: 10px;
}
</style>
