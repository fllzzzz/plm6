<template>
  <div>
    <div class="batch-operation" style="display: flex; justify-content: space-between; margin-bottom: 8px">
      <el-tag type="primary" style="font-size: 14px; align-self: center">当前项目：{{ props.batchRow.projectName }}</el-tag>
      <div>
        <common-button type="danger" size="mini" :disabled="handleSelectionData.length === 0" @click="batchDel">批量删除</common-button>
        <common-button type="success" size="mini" :disabled="handleSelectionData.length === 0" @click="batchIssued">批量下发</common-button>
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
      <el-table-column type="selection" align="center" width="60" />
      <el-table-column key="batchNumber" prop="batchNumber" :show-overflow-tooltip="true" label="套料批次号" min-width="120" align="center">
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
      <el-table-column key="nestingTotalQuantity" prop="nestingTotalQuantity" :show-overflow-tooltip="true" label="套料总数" align="center">
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
          <el-tag :type="taskIssueTypeEnum.V[row.statusIssueEnum].T">{{ taskIssueTypeEnum.VL[row.statusIssueEnum] }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" label="操作" min-width="150px" align="center">
        <template #default="{ row }">
          <common-button type="primary" size="mini" @click="views(row)">查看</common-button>
          <el-popconfirm confirm-button-text="确定" cancel-button-text="取消" title="确定下发吗?" @confirm="issued(row)">
            <template #reference>
              <common-button size="mini" type="success" :disabled="row.statusIssueEnum === taskIssueTypeEnum.HAS_ISSUED.V">下发</common-button>
            </template>
          </el-popconfirm>
          <el-popconfirm
            confirm-button-text="确定"
            cancel-button-text="取消"
            icon-color="red"
            title="确定删除吗?"
            @confirm="delNestingResult(row)"
          >
            <template #reference>
              <common-button size="mini" type="danger">删除</common-button>
            </template>
          </el-popconfirm>
        </template>
      </el-table-column>
    </common-table>
    <pagination />
    <nesting-file v-model:visible="nestingFileVisible" :detail-data="detailData" />
  </div>
</template>

<script setup>
import { ref, defineProps, defineEmits, watch } from 'vue'
import { nestingBatchList } from '@/api/mes/craft-manage/section-steel/nesting-result'

import useMaxHeight from '@compos/use-max-height'
import { ElMessageBox, ElNotification } from 'element-plus'
import { taskIssueTypeEnum } from '@enum-ms/mes'
import { nestingBatchIssued, nestingBatchDel } from '@/api/mes/craft-manage/section-steel/nesting-result'
import pagination from '@crud/Pagination'
import nestingFile from '../nesting-file/index.vue'

const emit = defineEmits(['success'])
const props = defineProps({
  batchRow: {
    type: Object,
    default: () => {}
  }
})
const { maxHeight } = useMaxHeight({
  extraBox: ['.batch-operation'],
  paginate: true
})
const tableRef = ref()
const detailData = ref({})
const nestingFileVisible = ref(false)
const handleSelectionData = ref([])
const batchList = ref([])

watch(
  () => props.batchRow.id,
  (val) => {
    if (val) {
      fetchData()
    }
  },
  { deep: true, immediate: true }
)

async function fetchData() {
  if (!props.batchRow.id) {
    return
  }
  try {
    const { content } = await nestingBatchList({ id: props.batchRow.id })
    batchList.value = content
  } catch (error) {
    console.log('获取套料批次数据失败')
  }
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
.item-name {
  padding: 2px 8px;
  background-color: #ecf8ff;
  border-radius: 2px;
  border-left: 5px solid #50bfff;
  margin-left: 5px;
}
</style>
