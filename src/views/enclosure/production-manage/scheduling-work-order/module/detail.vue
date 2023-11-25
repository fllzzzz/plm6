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
      <common-button v-if="delButton" type="primary" size="mini" class="filter-item" @click="delButton = false">编辑</common-button>
      <div v-else>
        <common-button type="warning" size="mini" @click="cancelEdit">取消编辑</common-button>
        <el-popconfirm
          confirm-button-text="确定"
          cancel-button-text="取消"
          icon-color="red"
          title="是否确定批量撤回?"
          @confirm="batchRevocation"
        >
        <template #reference>
          <common-button type="danger" size="mini" :disabled="selectDisable">批量撤回</common-button>
        </template>
      </el-popconfirm>
      </div>
      <export-button
        v-permission="crud.permission.print"
        :fn="download"
        :params="{ id: detail?.rowDetail?.id }"
        class="filter-item"
        @download-success="downloadSuccess"
      >
        下载排产工单详情
      </export-button>
      <!-- <print-table
        v-permission="crud.permission.print"
        api-key="enclosureSchedulingWorkOrderDetail"
        :params="{ id: detail?.rowDetail?.id }"
        size="mini"
        type="warning"
        class="filter-item"
        @print-success="downloadSuccess"
      /> -->
    </template>
    <template #content>
      <common-table ref="tableRef" :data="detail.content" :max-height="maxHeight" :data-format="dataFormat" show-summary :summary-method="getSummaries" @selection-change="selectChange">
        <el-table-column type="selection" :selectable="selectable" width="55" align="center" />
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="planName" prop="planName" label="批次" show-overflow-tooltip align="center" />
        <el-table-column key="name" prop="name" show-overflow-tooltip label="名称" align="center" />
        <el-table-column key="serialNumber" prop="serialNumber" show-overflow-tooltip label="编号" align="center" />
        <el-table-column
          v-if="detail.rowDetail?.category !== mesEnclosureTypeEnum.FOLDING_PIECE.V"
          key="plate"
          prop="plate"
          show-overflow-tooltip
          label="板型"
          align="center"
        />
        <el-table-column key="thickness" prop="thickness" show-overflow-tooltip label="厚度(mm)" align="center" />
        <el-table-column key="brand" prop="brand" show-overflow-tooltip label="品牌" align="center" />
        <el-table-column key="color" prop="color" show-overflow-tooltip label="颜色" align="center" />
        <el-table-column key="unfoldedWidth" prop="unfoldedWidth" show-overflow-tooltip label="宽度(mm)" align="center" />
        <el-table-column key="length" prop="length" show-overflow-tooltip label="单长(mm)" align="center" />
        <el-table-column key="quantity" prop="quantity" show-overflow-tooltip label="数量(件)" align="center" />
        <el-table-column key="totalLength" prop="totalLength" show-overflow-tooltip label="总长度(m)" align="center" />
        <el-table-column
          v-if="detail.rowDetail?.category !== mesEnclosureTypeEnum.SANDWICH_BOARD.V"
          key="totalWeight"
          prop="totalWeight"
          show-overflow-tooltip
          label="总重量(kg)"
          align="center"
        />
        <el-table-column key="askCompleteTime" prop="askCompleteTime" show-overflow-tooltip label="完成日期" align="center" />
        <el-table-column
          v-if="detail.rowDetail?.category === mesEnclosureTypeEnum.FOLDING_PIECE.V"
          key="relativePath"
          prop="relativePath"
          show-overflow-tooltip
          label="图片"
          align="center"
        >
          <template v-slot="{ row }">
            <img v-if="row.relativePath" :src="row.relativePath" alt="加载失败" style="max-width: 100%; display: block" />
            <span v-else>未上传图片</span>
          </template>
        </el-table-column>
        <el-table-column label="操作" align="center">
          <template #default="{row}">
            <el-popconfirm
              confirm-button-text="确定"
              cancel-button-text="取消"
              icon-color="red"
              title="是否确认撤回操作?"
              @confirm="revocation(row)"
            >
            <template #reference>
              <common-button type="danger" size="mini" :disabled="row.booleReport === 1">撤回</common-button>
            </template>
          </el-popconfirm>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { report, download, workOrderRevocation } from '@/api/enclosure/production-manage/scheduling-work-order'
import { computed, defineProps, ref } from 'vue'
import { ElMessage } from 'element-plus'

import { tableSummary } from '@/utils/el-extra'
import { DP } from '@/settings/config'
import { mesEnclosureTypeEnum } from '@enum-ms/mes'

import { regDetail } from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import ExportButton from '@comp-common/export-button/index.vue'

const props = defineProps({
  project: {
    type: Object,
    default: () => {}
  }
})

const drawerRef = ref()
const tableRef = ref()
const delButton = ref(true)
const taskId = ref([])
const selectDisable = ref(true)
const dataFormat = ref([
  ['askCompleteTime', ['parse-time', '{y}-{m}-{d}']],
  ['thickness', ['to-fixed', DP.MES_ENCLOSURE_T__MM]],
  ['totalLength', ['to-fixed', DP.MES_ENCLOSURE_L__M]],
  ['totalWeight', ['to-fixed', DP.COM_WT__KG]]
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

// 下载成功
async function downloadSuccess() {
  try {
    await report(detail?.rowDetail?.id)
  } catch (error) {
    console.log('下载成功上报失败')
  }
}

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: ['quantity', ['totalLength', DP.MES_ENCLOSURE_L__M], ['totalWeight', DP.COM_WT__KG]]
  })
}

// 取消编辑时复选框清空
const cancelEdit = () => {
  delButton.value = true
  tableRef.value.clearSelection()
}

// 点击编辑时，可选择复选框
function selectable(row) {
  return !delButton.value && row.booleReport !== 1
}

const selectChange = (v) => {
  console.log(v)
  if (v.length > 0) {
    selectDisable.value = false
    taskId.value = []
    v.forEach(item => {
      taskId.value.push(item.id)
    })
  } else {
    selectDisable.value = true
  }
}

// 批量撤回
const batchRevocation = async () => {
  try {
    await workOrderRevocation({ orderId: detail.rowDetail.id, taskIds: taskId.value })
    delButton.value = true
    crud.toDetail({ ...detail.rowDetail })
    ElMessage.success(`批量工单撤回成功`)
  } catch (error) {
    console.log(error, '批量撤回失败')
  }
}

// 撤回
const revocation = async (row) => {
  taskId.value = []
  try {
    taskId.value.push(row.id)
    await workOrderRevocation({ orderId: detail.rowDetail.id, taskIds: taskId.value })
    crud.toDetail({ ...detail.rowDetail })
    ElMessage.success(`当前工单撤回成功`)
  } catch (error) {
    console.log(error, '撤回失败')
  }
}

// 详情加载后
CRUD.HOOK.beforeDetailLoaded = async (crud) => {
  (detail.content || []).forEach((row) => {
    row.totalLength = (row.totalLength || 0) / 1000
  })
  if (detail.content?.length === 0) {
    crud.cancelDetail()
    crud.toQuery()
  }
}
</script>
