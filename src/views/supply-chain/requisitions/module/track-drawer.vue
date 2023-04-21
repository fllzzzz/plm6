<template>
  <common-drawer
    ref="drawerRef"
    :title="`申购单号：${info?.serialNumber}`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="100%"
  >
    <template #titleAfter>
      <span v-if="info?.project"> 【{{ projectNameFormatter(info?.project) }}】 </span>
    </template>
    <template #titleRight>
      <export-button size="mini" :params="{ id: props.info?.id }" :fn="exportScheduleExcel"> 下载 </export-button>
    </template>
    <template #content>
      <el-row v-if="info?.materialType === materialPurchaseClsEnum.STEEL.V" :gutter="20" class="panel-group">
        <el-col :span="8" class="card-panel-col">
          <Panel name="申购计划(kg)" text-color="#626262" num-color="#1890ff" :end-val="info?.totalMete || 0" :precision="2" />
        </el-col>
        <el-col :span="8" class="card-panel-col">
          <Panel name="实际入库(kg)" text-color="#626262" num-color="#1890ff" :end-val="info?.inboundMete || 0" :precision="2" />
        </el-col>
        <el-col :span="8" class="card-panel-col">
          <Panel name="入库率(%)" text-color="#626262" num-color="#1890ff" :end-val="info?.inboundRate || 0" :precision="2" />
        </el-col>
      </el-row>
      <div style="display: flex">
        <div style="flex: 1">
          <div class="table-title" style="background-color: #ecf5ff">
            <span>申购数据</span>
          </div>
          <common-table v-loading="contentLoading" :data="requisitionsList" :max-height="maxHeight" style="width: 100%">
            <!-- 基础信息 -->
            <material-base-info-columns />
            <!-- 单位及其数量 -->
            <material-unit-quantity-columns />
          </common-table>
        </div>

        <div style="flex: 1">
          <div class="table-title" style="background-color: #f5ffef">
            <span>采购数据</span>
          </div>
          <common-table v-loading="contentLoading" :data="inboundList" :max-height="maxHeight" style="width: 100%">
            <!-- 基础信息 -->
            <material-base-info-columns />
            <!-- 单位及其数量 -->
            <material-unit-quantity-columns />
          </common-table>
        </div>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { getSchedule, exportScheduleExcel } from '@/api/supply-chain/requisitions-manage/requisitions'
import { defineProps, defineEmits, ref } from 'vue'

import { projectNameFormatter } from '@/utils/project'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { materialPurchaseClsEnum } from '@enum-ms/classification'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import Panel from '@/components/Panel'
import ExportButton from '@comp-common/export-button/index.vue'
import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'

const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  info: {
    type: Object,
    default: () => ({})
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook })

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header', 'panel-group'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true
  },
  drawerVisible
)

const detailInfo = ref({})
const requisitionsList = ref([])
const inboundList = ref([])
const contentLoading = ref(false)

function showHook() {
  fetchList()
}

async function fetchList() {
  requisitionsList.value = []
  inboundList.value = []
  detailInfo.value = {}
  if (!props.info.id) return
  try {
    contentLoading.value = true
    const data = await getSchedule(props.info.id)
    await setSpecInfoToList(data.detailList)
    await numFmtByBasicClass(data.detailList)
    await setSpecInfoToList(data.inboundList)
    await numFmtByBasicClass(data.inboundList)
    requisitionsList.value = data.detailList
    inboundList.value = data.inboundList
    detailInfo.value = data
  } catch (e) {
    console.log(e)
  } finally {
    contentLoading.value = false
  }
}
</script>

<style lang="scss" scoped>
.table-title {
  height: 45px;
  line-height: 45px;
  display: flex;
  border: 1px solid #ebeef5;
  border-bottom-width: 0;
  font-size: 14px;
  font-weight: 600;
  color: #909399;
  justify-content: center;
}
.panel-group {
  margin-bottom: 20px;

  ::v-deep(.card-panel) {
    .card-panel-description {
      margin: 10px 20px;
      display: flex;
      flex-direction: row;
      justify-content: space-between;
      align-items: flex-start;
      flex-wrap: wrap;
      .card-panel-text {
        margin-top: 2px;
      }
      .card-panel-num {
        font-size: 20px;
      }
    }
  }

  .card-panel-col {
    cursor: pointer;
  }
}
</style>
