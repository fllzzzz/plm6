<template>
  <common-table
    :header-cell-style="() => `font-weight: bold;color: #333333;`"
    :data="newAssembleList"
    return-source-data
    style="max-width: 1100px"
    row-key="serialNumber"
    class="assemble-change-set-table"
    :expand-row-keys="expandRowKeys"
  >
    <el-expand-table-column :data="newAssembleList" v-model:expand-row-keys="expandRowKeys" row-key="serialNumber">
      <template #default="{ row }">
        <div class="set-con" v-if="row?.oldSerialNumbers?.length">
          <div v-for="item in row?.oldSerialNumbers" :key="item" class="set-item">
            <div class="diff-con">
              <div class="diff-item">
                <span style="flex: 0.5; text-align: left">{{ item }} -> {{ row.serialNumber }}</span>
                <span style="flex: 0.25">{{ needHandleOldObj[item]?.length }}</span>
                <span style="width: 90px">
                  <span v-if="row?.oldSerialNumbers?.length === 1">{{ needHandleOldObj[item]?.quantity }}</span>
                  <el-input-number
                    v-else
                    v-model.number="row.handleObj[item].quantity"
                    :min="1"
                    :max="needHandleOldObj.value[oldSN].quantity"
                    :step="1"
                    placeholder="数量"
                    style="width: 100%"
                  />
                </span>
                <span style="flex: 0.25">{{ needHandleOldObj[item]?.netWeight }}</span>
              </div>
              <div class="diff-item">
                <span style="flex: 0.5; text-align: left">差异</span>
                <cell-compare-preview
                  style="flex: 0.25"
                  :value="toPrecision((needHandleOldObj[item]?.length || 0) - (needHandleNewObj[row.serialNumber]?.length || 0))"
                />
                <cell-compare-preview
                  style="width: 90px"
                  :value="toPrecision((needHandleOldObj[item]?.quantity || 0) - (needHandleNewObj[row.serialNumber]?.quantity || 0))"
                />
                <cell-compare-preview
                  style="flex: 0.25"
                  :value="toPrecision((needHandleOldObj[item]?.netWeight || 0) - (needHandleNewObj[row.serialNumber]?.netWeight || 0), 2)"
                />
              </div>
            </div>
            <div class="method-opt">
              <div
                v-for="t in assembleHandleMethodEnum.ENUM"
                :key="t.V"
                class="method-item"
                :class="{ 'is-active': Boolean(row.handleObj?.[item]?.handleType & t.V) }"
                @click="handleTypeChange(row, item, t.V)"
              >
                {{ t.L }}
              </div>
            </div>
          </div>
        </div>
      </template>
    </el-expand-table-column>
    <el-table-column label="编号" prop="serialNumber" show-overflow-tooltip align="center" min-width="100">
      <template #default="{ row }">
        <span style="font-weight: bold">{{ row.serialNumber }}</span>
      </template>
    </el-table-column>
    <el-table-column label="规格" prop="specification" show-overflow-tooltip align="left" min-width="120" />
    <el-table-column label="长度(mm)" prop="length" show-overflow-tooltip align="center" min-width="110" />
    <el-table-column label="数量" prop="quantity" show-overflow-tooltip align="center" width="90" />
    <el-table-column label="单重(kg)" prop="netWeight" show-overflow-tooltip align="center" min-width="110" />
    <el-table-column label="操作" align="center" width="160">
      <template #default="{ row }">
        <common-radio
          v-model="row.operateType"
          :options="assembleOperateTypeEnum.ENUM"
          type="enum"
          @change="handleOperateTypeChange($event, row)"
        />
      </template>
    </el-table-column>
    <el-table-column label="旧部件编号" align="center" width="180">
      <template #default="{ row, $index }">
        <common-select
          v-if="row.operateType !== assembleOperateTypeEnum.NEW.V"
          v-model="row.oldSerialNumbers"
          :options="oldAssembleList"
          type="other"
          placeholder="请选择"
          :disabledVal="getDisabledList($index)"
          :data-structure="{ key: 'serialNumber', label: 'serialNumber', value: 'serialNumber' }"
          clearable
          collapseTags
          multiple
          style="width: 100%"
          @change="handleSerialNumberChange($event, row)"
        />
        <span v-else>/</span>
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { computed, defineProps, ref } from 'vue'

import { toPrecision } from '@/utils/data-type'
import { assembleOperateTypeEnum, assembleHandleMethodEnum } from './common'

import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import cellComparePreview from '@comp-common/cell-compare-preview'

const props = defineProps({
  newAssembleList: {
    type: Array,
    default: () => []
  },
  oldAssembleList: {
    type: Array,
    default: () => []
  },
  assembleInfo: {
    type: Object,
    default: () => {}
  }
})

const expandRowKeys = ref([])
const needHandleOldObj = computed(() => props.assembleInfo?.needHandleOldObj || {})
const needHandleNewObj = computed(() => props.assembleInfo?.needHandleNewObj || {})

function getDisabledList(index) {
  let _list = []
  for (let i = 0; i < props.newAssembleList.length; i++) {
    if (index === i) continue
    const row = props.newAssembleList[i]
    if (typeof row.oldSerialNumbers !== 'object') row.oldSerialNumbers = []
    const _rowDisabledSNs = []
    for (const oldSN of row.oldSerialNumbers) {
      if (row.handleObj[oldSN].quantity === needHandleOldObj[oldSN].quantity) {
        _rowDisabledSNs.push(oldSN)
      }
    }
    _list = [..._list, ..._rowDisabledSNs]
  }
  return _list
}

function handleOperateTypeChange(val, row) {
  if (val === assembleOperateTypeEnum.NEW.V) {
    row.oldSerialNumbers = []
  }
}

function handleSerialNumberChange(val, row) {
  if (val?.length && !expandRowKeys.value.includes(row.serialNumber)) {
    expandRowKeys.value.push(row.serialNumber)
  }
  if (val?.length && !row.operateType) {
    row.operateType = assembleOperateTypeEnum.EDIT.V
  }
  if (!row.handleObj || !val?.length) row.handleObj = {}
  if (val?.length) {
    for (const oldSN of val) {
      if (!row.handleObj[oldSN]) {
        row.handleObj[oldSN] = { quantity: needHandleOldObj.value[oldSN].quantity, netWeight: needHandleOldObj.value[oldSN].netWeight }
      }
    }
  }
}

function handleTypeChange(row, oldSn, val) {
  if (!(row.handleObj[oldSn]?.handleType & val)) {
    row.handleObj[oldSn].handleType |= val
  } else {
    row.handleObj[oldSn].handleType -= val
  }
}
</script>

<style lang="scss" scoped>
.assemble-change-set-table {
  ::v-deep(.el-radio) {
    margin-right: 15px;
  }
}

.set-con {
  padding: 15px 35px;
  .set-item:not(:last-child) {
    margin-bottom: 10px;
  }

  .set-item {
    border: 1px dashed #797979;
    padding: 10px 20px;
    display: flex;
    align-items: center;
    justify-content: space-between;

    .method-opt {
      display: flex;

      .method-item {
        width: 50px;
        height: 29px;
        line-height: 29px;
        padding: 0px 6px 0px 8px;
        text-align: center;
        cursor: pointer;
        color: #fff;
        font-weight: bold;
        background-color: #cdcdcd;
        border-radius: 4px;
      }

      .method-item:not(:last-child) {
        margin-right: 8px;
      }

      .is-active {
        background-color: #0066ff;
      }
    }
  }

  .diff-con {
    flex: 1;
    margin-right: 45px;
    .diff-item {
      display: flex;

      span {
        text-align: center;
      }
    }

    .diff-item:not(:last-child) {
      margin-bottom: 5px;
    }
  }
}
</style>
