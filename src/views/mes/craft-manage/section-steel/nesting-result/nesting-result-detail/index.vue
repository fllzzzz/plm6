<template>
  <div class="app-container">
    <div v-show="!props.batchRow.id" class="my-code">*点击左侧项目行查看详情</div>
    <div v-show="props.batchRow.id">
      <div class="batch-operation head-container" style="display: flex; justify-content: space-between">
        <el-tag class="filter-item" size="medium" style="align-self: center">当前项目：{{ props.batchRow.projectName }}</el-tag>
        <div class="filter-item">
          <common-button
            type="danger"
            v-permission="permission.del"
            size="mini"
            :disabled="handleSelectionData.length === 0"
            @click="batchDel"
          >
            批量删除
          </common-button>
          <common-button
            type="success"
            size="mini"
            v-permission="permission.issued"
            :disabled="handleSelectionData.length === 0"
            @click="batchIssued"
          >
            批量下发
          </common-button>
        </div>
      </div>
      <common-table
        ref="tableRef"
        :data="batchList"
        :empty-text="'暂无数据'"
        :max-height="maxHeight"
        row-key="id"
        style="width: 100%"
        @selection-change="handleSelectionChange"
      >
        <el-table-column type="selection" align="center" width="60" class="selection" :selectable="selectable" />
        <el-table-column key="batchNumber" prop="batchNumber" :show-overflow-tooltip="true" label="套料批次号" align="center">
          <template #default="{ row }">
            <span>{{ row.batchNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column key="userName" prop="userName" :show-overflow-tooltip="true" label="套料操作人" align="center">
          <template #default="{ row }">
            <span>{{ row.userName }}</span>
          </template>
        </el-table-column>
        <el-table-column
          key="assembleTotalQuantity"
          prop="assembleTotalQuantity"
          :show-overflow-tooltip="true"
          label="部件总数"
          align="center"
        >
          <template #default="{ row }">
            <span>{{ row.assembleTotalQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column
          key="assembleTotalNetWeight"
          prop="assembleTotalNetWeight"
          :show-overflow-tooltip="true"
          label="部件总重"
          align="center"
        >
          <template #default="{ row }">
            <span>{{ row.assembleTotalNetWeight }}</span>
          </template>
        </el-table-column>
        <el-table-column
          key="nestingTotalQuantity"
          prop="nestingTotalQuantity"
          :show-overflow-tooltip="true"
          label="套料总数"
          align="center"
        >
          <template #default="{ row }">
            <span>{{ row.nestingTotalQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column
          key="typesettingTotalNetWeight"
          prop="typesettingTotalNetWeight"
          :show-overflow-tooltip="true"
          label="母材总重"
          align="center"
        >
          <template #default="{ row }">
            <span>{{ row.typesettingTotalNetWeight }}</span>
          </template>
        </el-table-column>
        <el-table-column key="statusIssueEnum" prop="statusIssueEnum" :show-overflow-tooltip="true" label="状态" align="center">
          <template #default="{ row }">
            <el-tag :type="typeEnum.V[row.statusIssueEnum].T">{{ typeEnum.VL[row.statusIssueEnum] }}</el-tag>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" label="操作" min-width="200px" align="center">
          <template #default="{ row }">
            <common-button v-permission="permission.detail" type="primary" size="mini" @click="views(row)">查看</common-button>
            <el-popconfirm confirm-button-text="确定" cancel-button-text="取消" title="确定下发吗?" @confirm="issued(row)">
              <template #reference>
                <common-button
                  size="mini"
                  v-permission="permission.issued"
                  type="success"
                  :disabled="row.statusIssueEnum === typeEnum.ISSUED.V || row.statusIssueEnum === typeEnum.EXPIRED.V"
                >
                  下发
                </common-button>
              </template>
            </el-popconfirm>
            <export-button
              type="warning"
              v-permission="permission.downloadZip"
              size="mini"
              :params="{ id: row.id }"
              :fn="downloadZipGet"
              :icon="''"
              >下载</export-button
            >
            <el-popconfirm
              confirm-button-text="确定"
              cancel-button-text="取消"
              icon-color="red"
              title="确定删除吗?"
              @confirm="delNestingResult(row)"
            >
              <template #reference>
                <common-button
                  size="mini"
                  v-permission="permission.del"
                  type="danger"
                  :disabled="row.statusIssueEnum === typeEnum.ISSUED.V || row.statusIssueEnum === typeEnum.PRODUCTION.V"
                >
                  删除
                </common-button>
              </template>
            </el-popconfirm>
          </template>
        </el-table-column>
      </common-table>
      <nesting-file v-model:visible="nestingFileVisible" :detail-data="detailData" />
    </div>
  </div>
</template>

<script setup>
import { ref, defineProps, defineEmits, watch } from 'vue'
import { nestingBatchList, downloadZipGet } from '@/api/mes/craft-manage/section-steel/nesting-result'

import useMaxHeight from '@compos/use-max-height'
import { ElMessageBox, ElNotification } from 'element-plus'
import { MesBuildingTypesettingStatusEnum as typeEnum } from '@enum-ms/mes'
import { nestingBatchIssued, nestingBatchDel } from '@/api/mes/craft-manage/section-steel/nesting-result'
import { mesNestingResultPM as permission } from '@/page-permission/mes'
import ExportButton from '@comp-common/export-button/index.vue'
import nestingFile from '../nesting-file/index.vue'

const emit = defineEmits(['success'])
const props = defineProps({
  batchRow: {
    type: Object,
    default: () => {}
  }
})
const { maxHeight } = useMaxHeight({
  paginate: true
})
const tableRef = ref()
const detailData = ref({})
const nestingFileVisible = ref(false)
const handleSelectionData = ref([])
const batchList = ref([])

watch(
  () => props.batchRow?.id,
  (val) => {
    if (val) {
      fetchData()
    }
  },
  { deep: true, immediate: true }
)

async function fetchData() {
  if (!props.batchRow?.id) {
    return
  }
  try {
    const { content } = await nestingBatchList({ id: props.batchRow?.id })
    batchList.value = content
  } catch (error) {
    console.log('获取套料批次数据失败')
  }
}

function selectable(row, rowIndex) {
  return row.statusIssueEnum === typeEnum.COMPLETE.V
}
// 选中进行批量操作
function handleSelectionChange(val) {
  handleSelectionData.value = val
}

// 批量删除
function batchDel() {
  ElMessageBox.confirm('是否确认批量删除选中数据的套料成果？', {
    confirmButtonText: '确定',
    cancelButtonText: '取消',
    type: 'warning',
    draggable: true
  })
    .then(async () => {
      const _data = []
      handleSelectionData.value.map((v) => {
        _data.push(v.id)
      })
      await nestingBatchDel(_data)
      fetchData()
      emit('success')
      ElNotification({
        title: '批量删除成功',
        type: 'success',
        duration: 2500
      })
    })
    .catch(() => {
      console.log('删除失败')
    })
}

// 批量下发
function batchIssued() {
  ElMessageBox.confirm('是否确认批量下发选中数据？', {
    confirmButtonText: '确定',
    cancelButtonText: '取消',
    type: 'warning',
    draggable: true
  })
    .then(async () => {
      const _data = []
      handleSelectionData.value.map((v) => {
        _data.push(v.id)
      })
      await nestingBatchIssued(_data)
      fetchData()
      emit('success')
      ElNotification({
        title: '批量下发成功',
        type: 'success',
        duration: 2500
      })
    })
    .catch(() => {
      console.log('批量下发失败')
    })
}

// 查看
function views(row) {
  nestingFileVisible.value = true
  detailData.value = row
}

// 下发
async function issued(row) {
  try {
    const _data = []
    _data.push(row.id)
    await nestingBatchIssued(_data)
    fetchData()
    emit('success')
    ElNotification({
      title: '下发成功',
      type: 'success',
      duration: 2500
    })
  } catch (error) {
    console.log('下发失败')
  }
}

// 删除
async function delNestingResult(row) {
  try {
    const _data = []
    _data.push(row.id)
    await nestingBatchDel(_data)
    fetchData()
    emit('success')
    ElNotification({
      title: '删除套料成果成功',
      type: 'success',
      duration: 2500
    })
  } catch (error) {
    console.log('删除套料成果失败')
  }
}
</script>

<style lang="scss" scoped>
.app-container {
  padding: 0;
}
</style>
