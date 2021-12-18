<template>
  <common-table v-bind="$attrs" v-loading="detailLoading" :data="list">
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column key="type" :show-overflow-tooltip="true" prop="type" label="类型" align="center" width="100">
      <template #default="{ row }">
        <span v-parse-enum="{ e: materialFreezeTypeEnum, v: row.freezeType }" v-suffix="'冻结'" />
      </template>
    </el-table-column>
    <el-table-column key="documentType" :show-overflow-tooltip="true" prop="documentType" label="对应单据" align="center" width="120">
      <template #default="{ row }">
        <span v-if="materialFreezeTypeEnum.V[row.freezeType]">{{ materialFreezeTypeEnum.V[row.freezeType].DOC }}</span>
      </template>
    </el-table-column>
    <el-table-column key="document" :show-overflow-tooltip="true" prop="document" label="单据编号" align="center" min-width="120">
      <template #default="{ row }">
        <clickable-permission-span
          v-if="row.document && row.document.serialNumber"
          :permission="openDetailPermission(row.freezeType)"
          @click="openDocumentDetail(row.freezeType, row.document.id)"
          :text="row.document.serialNumber"
        />
      </template>
    </el-table-column>
    <el-table-column show-overflow-tooltip key="project" prop="project" label="单据项目" min-width="170">
      <template #default="{ row }">
        <span v-parse-project="{ project: row.project }" v-empty-text />
      </template>
    </el-table-column>
    <el-table-column prop="outboundUnit" label="单位" align="center" width="70px">
      <span v-empty-text>{{ material.outboundUnit }}</span>
    </el-table-column>
    <el-table-column prop="curQuantity" label="数量" align="right" width="100px">
      <template #default="{ row }">
        <span v-empty-text v-to-fixed="material.outboundUnitPrecision">
          {{ material.curOutboundUnitType === measureTypeEnum.MEASURE.V ? row.quantity : row.mete }}
        </span>
      </template>
    </el-table-column>
    <el-table-column key="operatorName" :show-overflow-tooltip="true" prop="operatorName" label="冻结人" align="center" width="90" />
    <el-table-column key="frozenTime" :show-overflow-tooltip="true" prop="frozenTime" label="冻结日期" align="center" width="100">
      <template #default="{ row }">
        <span v-parse-time="'{y}-{m}-{d}'">{{ row.frozenTime }}</span>
      </template>
    </el-table-column>
    <el-table-column label="操作" width="85px" align="center">
      <template #default="{ row }">
        <common-button v-if="checkUnFreezePermission(row.freezeType)" type="primary" size="mini" @click="toUnfreeze(row)">
          解 冻
        </common-button>
        <span v-else>无权限</span>
      </template>
    </el-table-column>
  </common-table>
  <unfreeze-form v-model:visible="unfreezeFormVisible" :material="props.material" :record="currentRecord" @success="handleFreezeSuccess" />
  <!-- 调拨详情 -->
  <detail-wrapper ref="transferDetailRef" :api="getTransferDetail">
    <transfer-detail />
  </detail-wrapper>
  <detail-wrapper ref="outboundDetailRef" :api="getOutboundDetail">
    <outbound-detail />
  </detail-wrapper>
</template>

<script setup>
import { getMaterialFreezeRecordById } from '@/api/wms/freeze/raw-mat'
import { detail as getTransferDetail } from '@/api/wms/transfer/raw-mat-application-review'
import { detail as getOutboundDetail } from '@/api/wms/outbound/raw-mat-application-review'
import { defineEmits, defineProps, ref, watch } from 'vue'
import { materialFreezeTypeEnum, measureTypeEnum } from '@/utils/enum/modules/wms'
import checkPermission from '@/utils/system/check-permission'

import DetailWrapper from '@crud/detail-wrapper.vue'
import ClickablePermissionSpan from '@/components-system/common/clickable-permission-span.vue'
import TransferDetail from '@/views/wms/transfer-application-review/raw-mat/module/detail.vue'
import OutboundDetail from '@/views/wms/outbound-application-review/raw-mat/module/detail.vue'
import UnfreezeForm from '../components/unfreeze/index.vue'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'

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

// crud交由presenter持有
const permission = {
  requisitionsUnFreeze: ['wms_raw_mat_freeze_list:unfreeze_requisitions'],
  outboundUnFreeze: ['wms_raw_mat_freeze_list:unfreeze_outbound'],
  transferUnFreeze: ['wms_raw_mat_freeze_list:unfreeze_transfer'],
  transferDetail: ['wms_transferApplication_review:detail'],
  outboundDetail: ['wms_outboundApplication_review:detail'],
  requisitionsDetail: ['wms_requisitions:detail']
}

// 详情加载
const detailLoading = ref(false)
// 列表
const list = ref([])
// 调拨详情组件
const transferDetailRef = ref()
// 出库详情组件
const outboundDetailRef = ref()
// 解冻显示
const unfreezeFormVisible = ref(false)
// 当前解冻记录
const currentRecord = ref()

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
    const { content = [] } = await getMaterialFreezeRecordById()
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
function handleFreezeSuccess() {
  if (props.mode === 'fetch') {
    // 刷新
    fetchMaterialFreezeDetailById()
  }
  emit('unfreeze-success')
}

// 去解冻
function toUnfreeze(record) {
  unfreezeFormVisible.value = true
  currentRecord.value = record
}

// 打开详情
function openDocumentDetail(freezeType, id) {
  switch (freezeType) {
    case materialFreezeTypeEnum.REQUISITIONS.V:
      break
    case materialFreezeTypeEnum.OUTBOUND.V:
      outboundDetailRef.value.toDetail(id)
      break
    case materialFreezeTypeEnum.TRANSFER.V:
      // 打开调拨详情
      transferDetailRef.value.toDetail(id)
      break
  }
}

// 校验解冻权限
function checkUnFreezePermission(freezeType) {
  const permission = unfreezePermission(freezeType)
  return checkPermission(permission)
}

// 解冻权限
function unfreezePermission(freezeType) {
  switch (freezeType) {
    case materialFreezeTypeEnum.REQUISITIONS.V:
      return permission.requisitionsUnFreeze
    case materialFreezeTypeEnum.OUTBOUND.V:
      return permission.outboundUnFreeze
    case materialFreezeTypeEnum.TRANSFER.V:
      return permission.transferUnFreeze
  }
}

// 查看详情权限
function openDetailPermission(freezeType) {
  switch (freezeType) {
    case materialFreezeTypeEnum.REQUISITIONS.V:
      return permission.requisitionsDetail
    case materialFreezeTypeEnum.OUTBOUND.V:
      return permission.outboundDetail
    case materialFreezeTypeEnum.TRANSFER.V:
      return permission.transferDetail
  }
}
</script>

<style lang="scss" scoped></style>
