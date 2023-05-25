<template>
  <common-drawer
    ref="drawerRef"
    :visible="crud.detailVisible"
    :content-loading="crud.detailLoading"
    :before-close="beforeClose"
    :title="detail?.rowDetail?.orderNumber"
    :show-close="true"
    size="100%"
    custom-class="enclosure-scheduling-work-order-detail"
  >
    <template #titleAfter>
      <el-tag effect="plain" size="medium">
        <span v-parse-project="{ project: props.project }" v-empty-text />
      </el-tag>
      <el-tag type="success" effect="plain" size="medium">{{ detail?.rowDetail?.factoryName }}</el-tag>
      <el-tag type="success" effect="plain" size="medium">{{ detail?.rowDetail?.workshopName }}</el-tag>
      <el-tag type="success" effect="plain" size="medium">
        {{ detail?.rowDetail?.productionLineName }} / {{ detail?.rowDetail?.leaderName }}
      </el-tag>
    </template>
    <template #titleRight>
      <print-table
        v-permission="crud.permission.print"
        api-key="enclosureSchedulingWorkOrderDetail"
        :params="{ id: detail?.rowDetail?.id }"
        size="mini"
        type="warning"
        class="filter-item"
        @print-success="printSuccess"
      />
    </template>
    <template #content>
      <common-table :data="detail.content" :max-height="maxHeight" :data-format="dataFormat" show-summary :summary-method="getSummaries">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="planName" prop="planName" label="批次" show-overflow-tooltip align="center" />
        <el-table-column key="name" prop="name" show-overflow-tooltip label="名称" align="center" />
        <el-table-column key="serialNumber" prop="serialNumber" show-overflow-tooltip label="编号" align="center" />
        <el-table-column v-if="detail.rowDetail?.category !== mesEnclosureTypeEnum.FOLDING_PIECE.V" key="plate" prop="plate" show-overflow-tooltip label="板型" align="center" />
        <el-table-column key="brand" prop="brand" show-overflow-tooltip label="品牌" align="center" />
        <el-table-column key="color" prop="color" show-overflow-tooltip label="颜色" align="center" />
        <el-table-column key="length" prop="length" show-overflow-tooltip label="单长(mm)" align="center" />
        <el-table-column key="quantity" prop="quantity" show-overflow-tooltip label="数量(件)" align="center" />
        <el-table-column key="totalLength" prop="totalLength" show-overflow-tooltip label="总长度(m)" align="center" />
        <el-table-column key="askCompleteTime" prop="askCompleteTime" show-overflow-tooltip label="完成日期" align="center" />
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { report } from '@/api/enclosure/production-manage/scheduling-work-order'
import { computed, defineProps, ref } from 'vue'

import { tableSummary } from '@/utils/el-extra'
import { DP } from '@/settings/config'
import { mesEnclosureTypeEnum } from '@enum-ms/mes'

import { regDetail } from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'

const props = defineProps({
  project: {
    type: Object,
    default: () => {}
  }
})

const drawerRef = ref()
const dataFormat = ref([
  ['askCompleteTime', ['parse-time', '{y}-{m}-{d}']],
  ['totalLength', ['to-fixed', DP.MES_ENCLOSURE_L__M]]
])

const { CRUD, crud, detail } = regDetail()

// 表格高度处理
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.enclosure-scheduling-work-order-detail',
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    clientHRepMainH: true
  },
  () => computed(() => !crud.detailLoading)
)

// 关闭
function beforeClose() {
  crud.cancelDetail()
  crud.toQuery()
}

// 打印成功（预览和下载不算）
async function printSuccess() {
  try {
    await report(detail?.rowDetail?.id)
  } catch (error) {
    console.log('打印成功上报失败')
  }
}

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: [['totalLength', DP.MES_ENCLOSURE_L__M]]
  })
}

// 详情加载后
CRUD.HOOK.beforeDetailLoaded = async (crud) => {
  (detail.content || []).forEach((row) => {
    row.totalLength = (row.totalLength || 0) / 1000
  })
}
</script>
