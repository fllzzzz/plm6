<template>
  <common-table v-bind="$attrs" v-loading="detailLoading" :data="list" :data-format="columnsDataFormat" row-key="id">
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column key="freezeTypeName" :show-overflow-tooltip="true" prop="freezeTypeName" label="类型" align="center" width="100" />
    <el-table-column key="receiptType" :show-overflow-tooltip="true" prop="receiptType" label="对应单据" align="center" width="120">
      <template #default="{ row }">
        <span v-if="materialFreezeTypeEnum.V[row.freezeType]">{{ materialFreezeTypeEnum.V[row.freezeType].DOC }}</span>
      </template>
    </el-table-column>
    <el-table-column key="receipt" :show-overflow-tooltip="true" prop="receipt" label="单据编号" align="center" min-width="120">
      <template #default="{ row }">
        <receipt-sn-clickable :receipt-types="['PREPARATION', 'OUTBOUND_APPLY', 'TRANSFER', 'REJECTED']" :receipt="row.receipt" />
      </template>
    </el-table-column>
    <el-table-column show-overflow-tooltip key="project" prop="project" label="单据项目" min-width="170" />
    <el-table-column prop="outboundUnit" label="单位" align="center" width="70px">
      {{ material.outboundUnit }}
    </el-table-column>
    <el-table-column prop="curQuantity" label="数量" align="right" width="100px">
      <template #default="{ row }">
        {{ material.outboundUnitType === measureTypeEnum.MEASURE.V ? row.quantity : row.mete }}
      </template>
    </el-table-column>
    <el-table-column key="operatorName" :show-overflow-tooltip="true" prop="operatorName" label="冻结人" align="center" width="90" />
    <el-table-column key="frozenTime" :show-overflow-tooltip="true" prop="frozenTime" label="冻结日期" align="center" width="100" />
    <el-table-column label="操作" width="85px" align="center">
      <template #default="{ row: { sourceRow: row } }">
        <common-button v-if="checkUnFreezePermission(row.freezeType)" type="primary" size="mini" @click="toUnfreeze(row)">
          解 冻
        </common-button>
        <span v-else>无权限</span>
      </template>
    </el-table-column>
  </common-table>
  <unfreeze-form v-model:visible="unfreezeFormVisible" :material="props.material" :record="currentRecord" @success="handleFreezeSuccess" />
</template>

<script setup>
import { getMaterialFreezeRecordById } from '@/api/wms/material-freeze/raw-material/record'
import { materialFreezeRecordCPM as permission } from '@/page-permission/wms'

import { defineEmits, defineProps, ref, watch } from 'vue'
import { materialFreezeTypeEnum, measureTypeEnum } from '@/utils/enum/modules/wms'
import checkPermission from '@/utils/system/check-permission'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { materialColumns } from '@/utils/columns-format/wms'
import UnfreezeForm from '../components/unfreeze/index.vue'
import ReceiptSnClickable from '@/components-system/wms/receipt-sn-clickable'

const emit = defineEmits(['unfreeze-success'])

const props = defineProps({
  material: {
    type: Object,
    default: () => {
      return {}
    }
  },
  // 模式
  mode: {
    type: String, // fetch(拉取) / incoming(传入)
    default: 'fetch'
  },
  records: {
    type: Array,
    default: () => []
  }
})

// 详情加载
const detailLoading = ref(false)
// 列表
const list = ref([])
// 解冻显示
const unfreezeFormVisible = ref(false)
// 当前解冻记录
const currentRecord = ref()
// 表格列数据格式转换
const columnsDataFormat = ref([
  ...materialColumns,
  ['frozenTime', ['parse-time', '{y}-{m}-{d}']],
  ['freezeTypeName', ['parse-enum', materialFreezeTypeEnum], ['suffix', '冻结'], { source: 'freezeType' }]
])
if (props.mode === 'fetch') {
  watch(
    () => props.material,
    (val) => {
      fetchMaterialFreezeDetailById(val)
    },
    { immediate: true }
  )
}

if (props.mode === 'incoming') {
  watch(
    () => props.records,
    async (val) => {
      list.value = await dataUnitFormat(props.material, val)
    },
    { immediate: true }
  )
}

// 加载物料冻结详情
async function fetchMaterialFreezeDetailById() {
  const material = props.material
  if (!material || !material.id) return
  try {
    list.value = []
    detailLoading.value = true
    const { content = [] } = await getMaterialFreezeRecordById(material.id)
    list.value = await dataUnitFormat(material, content)
  } catch (error) {
    console.error('error', error)
  } finally {
    detailLoading.value = false
  }
}

async function dataUnitFormat(material, recordList = []) {
  if (!material || recordList.length === 0) {
    return recordList
  }
  return await numFmtByBasicClass(recordList, {
    measureUnit: material.measureUnit,
    accountingUnit: material.accountingUnit,
    accountingPrecision: material.accountingPrecision,
    measurePrecision: material.measurePrecision,
    toSmallest: false,
    toNum: true
  })
}

// 处理解冻成功
function handleFreezeSuccess(data) {
  if (props.mode === 'fetch') {
    // 刷新
    fetchMaterialFreezeDetailById()
  }
  emit('unfreeze-success', data, currentRecord.value, props.material)
}

// 去解冻
function toUnfreeze(record) {
  unfreezeFormVisible.value = true
  currentRecord.value = record
}

// 校验解冻权限
function checkUnFreezePermission(freezeType) {
  const permission = unfreezePermission(freezeType)
  return checkPermission(permission)
}

// 解冻权限
function unfreezePermission(freezeType) {
  switch (freezeType) {
    case materialFreezeTypeEnum.PREPARATION.V:
      return permission.preparationUnFreeze
    case materialFreezeTypeEnum.OUTBOUND_APPLY.V:
      return permission.outboundUnFreeze
    case materialFreezeTypeEnum.TRANSFER.V:
      return permission.transferUnFreeze
    case materialFreezeTypeEnum.REJECTED.V:
      return permission.rejectUnFreeze
  }
}
</script>

<style lang="scss" scoped></style>
