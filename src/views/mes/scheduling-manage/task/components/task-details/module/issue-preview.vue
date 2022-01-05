<template>
  <common-dialog title="任务下发预览" v-model="dialogVisible" append-to-body :before-close="handleClose" :width="'70%'">
    <template #titleAfter>
      <el-tooltip
        effect="light"
        :content="`预览定义：\n
                1. /：表示该项未修改；\n`"
        placement="right"
      >
        <div style="display: inline-block">
          <i class="el-icon-info" />
        </div>
      </el-tooltip>
    </template>
    <template #titleRight>
      <common-button :loading="loading" size="mini" :disabled="!modifiedData || modifiedData.length == 0" type="primary" @click="submit">
        保 存
      </common-button>
    </template>
    <common-table :data="modifiedData" :max-height="maxHeight" empty-text="未做改动" style="width: 100%">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column prop="productionLine.name" :show-overflow-tooltip="true" label="生产线" min-width="140px">
        <template v-slot="scope">
          <span>{{ emptyTextFormatter(scope.row.workshop?.name) }}>{{ emptyTextFormatter(scope.row.productionLine?.name) }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="project.shortName" :show-overflow-tooltip="true" label="所属项目" min-width="180px" :fixed="fixed">
        <template #default="{ row }">
          <span v-parse-project="{ project: row.project }" v-empty-text />
        </template>
      </el-table-column>
      <el-table-column prop="area.name" :show-overflow-tooltip="true" label="单体区域" min-width="140px">
        <template v-slot="scope">
          <span>{{ emptyTextFormatter(scope.row.monomer?.name) }}>{{ emptyTextFormatter(scope.row.area?.name) }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="serialNumber" :show-overflow-tooltip="true" label="编号" min-width="100px">
        <template v-slot="scope">
          <span>{{ emptyTextFormatter(scope.row.serialNumber) }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="specification" :show-overflow-tooltip="true" label="规格" min-width="110px">
        <template v-slot="scope">
          <span>{{ emptyTextFormatter(scope.row.specification) }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="sourceSchedulingQuantity" label="总任务数" min-width="100px">
        <template v-slot="scope">
          <span>{{ emptyTextFormatter(scope.row.sourceSchedulingQuantity) }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="schedulingQuantity" label="下发任务数" min-width="100px">
        <template v-slot="scope">
          <span v-if="scope.row.schedulingQuantity === scope.row.sourceSchedulingQuantity" style="color: #67c23a">{{
            emptyTextFormatter(scope.row.schedulingQuantity)
          }}</span>
          <span v-else style="color: red">{{ emptyTextFormatter(scope.row.schedulingQuantity) }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="askCompleteTime" label="要求完成日期" align="center" min-width="110px">
        <template v-slot="scope">
          <span>{{ emptyTextFormatter(parseTime(scope.row.askCompleteTime, '{y}-{m}-{d}')) }}</span>
        </template>
      </el-table-column>
    </common-table>
  </common-dialog>
</template>

<script setup>
import { taskIssue } from '@/api/mes/scheduling-manage/task/common'
import { defineEmits, defineProps, ref } from 'vue'
import { ElNotification } from 'element-plus'
import moment from 'moment'

import { parseTime } from '@/utils/date'
import { emptyTextFormatter } from '@data-type'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const emit = defineEmits(['refresh', 'update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  modifiedData: {
    type: Array,
    default: () => []
  }
})

const loading = ref(false)

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    minHeight: 300,
    navbar: false
  },
  dialogVisible
)

async function submit() {
  try {
    loading.value = true
    const list = []
    props.modifiedData.forEach((v) => {
      list.push({
        schedulingId: v.id,
        taskQuantity: v.schedulingQuantity,
        askCompleteTime: moment(v.askCompleteTime).valueOf()
      })
    })
    await taskIssue({
      taskList: list
    })
    ElNotification({ title: '任务下发成功', type: 'success' })
    handleClose()
    emit('refresh')
  } catch (error) {
    console.log('操作失败', error)
  } finally {
    loading.value = false
  }
}
</script>
