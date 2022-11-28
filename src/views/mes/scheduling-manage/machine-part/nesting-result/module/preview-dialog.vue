<template>
  <common-dialog
    title="任务下发预览"
    customClass="nesting-task-issue-preview"
    v-model="dialogVisible"
    :close-on-click-modal="false"
    width="1100px"
    :showClose="false"
    :before-close="handleClose"
  >
    <template #titleRight>
      <common-button size="mini" type="primary" :loading="issueLoading" @click="submitIt"> 确认下发 </common-button>
      <common-button size="mini" @click="handleClose"> 取消 </common-button>
    </template>
    <div class="tip">
      <span>* 提示：</span>
      <span> 点击确认下发后，任务无法撤回，请再次确认！</span>
    </div>
    <common-table :data="list" :data-format="dataFormat" :max-height="maxHeight" style="width: 100%">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column prop="cutNumber" :show-overflow-tooltip="true" label="切割指令号" min-width="120" align="center" />
      <el-table-column
        v-if="info.boolNestCutEnum"
        prop="spec"
        :show-overflow-tooltip="true"
        label="原材料规格"
        min-width="120"
        align="center"
      />
      <el-table-column
        v-if="info.boolNestCutEnum"
        prop="num"
        :show-overflow-tooltip="true"
        label="板材数量(件)"
        width="120"
        align="center"
      />
      <el-table-column prop="cutName" :show-overflow-tooltip="true" label="切割方式" width="100" align="center" />
      <el-table-column :show-overflow-tooltip="true" label="生产组" min-width="160" align="center">
        <template #default="{ row }">
          <span>{{ row.workshop?.name }}>{{ row.productionLine?.name }}>{{ row.groupsName }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="askCompleteTime" :show-overflow-tooltip="true" label="需求完成日期" width="100" align="center" />
    </common-table>
  </common-dialog>
</template>

<script setup>
import { saveTask } from '@/api/mes/scheduling-manage/common'
import { defineEmits, defineProps, ref } from 'vue'
import { ElNotification } from 'element-plus'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const emit = defineEmits(['update:visible', 'success'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  list: {
    type: Array,
    default: () => []
  },
  info: {
    type: Object,
    default: () => {}
  }
})

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.nesting-task-issue-preview',
    extraBox: ['.el-dialog__header', '.tip'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    minHeight: 300,
    navbar: false
  },
  dialogVisible
)

const dataFormat = ref([['askCompleteTime', ['parse-time', '{y}-{m}-{d}']]])
const issueLoading = ref(false)

async function submitIt() {
  try {
    issueLoading.value = true
    const _resList = props.list.map((v) => {
      return {
        askCompleteTime: v.askCompleteTime,
        groupsId: v.groupsId,
        id: v.id,
        nestCutPlateId: v.nestCutPlateId
      }
    })
    await saveTask({
      machinePartDetailList: _resList
    })
    handleClose()
    ElNotification({ title: '任务下发成功', type: 'success', duration: 3000 })
    emit('success')
  } catch (e) {
    console.log(`任务下发失败`, e)
  } finally {
    issueLoading.value = false
  }
}
</script>

<style scoped>
.tip {
  display: inline-block;
  color: red;
  text-decoration: underline;
  margin-bottom: 10px;
}
</style>
