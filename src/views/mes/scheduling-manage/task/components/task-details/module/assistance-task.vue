<template>
  <common-dialog
    append-to-body
    title="协同任务"
    :close-on-click-modal="false"
    :before-close="handleClose"
    :visible="dialogVisible"
    width="600px"
  >
    <template #titleRight>
      <common-button :loading="submitLoading" size="mini" type="primary" @click="submitIt">确认</common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="110px">
      <el-form-item label="编号">
        <span v-empty-text>{{ details.serialNumber }}</span>
      </el-form-item>
      <el-form-item label="所属项目">
        <span v-empty-text>{{ details.project?.name }}</span>
      </el-form-item>
      <el-form-item label="任务数">
        <span>{{ details.sourceSchedulingQuantity }}</span>
      </el-form-item>
      <el-form-item label="未生产数">
        <span>{{ unInProductionQuantity }}</span>
      </el-form-item>
      <el-form-item label="选择生产线班组" prop="teamId">
        <common-select
          v-model="form.productionLineId"
          :options="lines"
          type="other"
          :loading="teamListLoading"
          size="small"
          clearable
          placeholder="请选择生产线"
          style="width: 150px"
        />
        <common-select
          v-model="form.teamId"
          :options="linesMap[form.productionLineId]?.teamList || []"
          type="other"
          size="small"
          clearable
          :noDataText="form.productionLineId ? '暂无数据' : '未选择生产线'"
          placeholder="请选择班组"
          style="width: 150px; margin-left: 5px"
        >
        </common-select>
      </el-form-item>
      <!-- <el-form-item label="协同数量" prop="quantity">
        <el-input-number
          v-model="form.quantity"
          :step="1"
          :min="0"
          :max="unInProductionQuantity"
          size="small"
          controls-position="right"
          style="width: 305px"
        />
      </el-form-item> -->
    </el-form>
  </common-dialog>
</template>

<script setup>
import { add, teamList } from '@/api/mes/scheduling-manage/task/assistance'
import { defineProps, ref, defineEmits, computed, reactive, watch } from 'vue'
import { ElNotification } from 'element-plus'

import useVisible from '@compos/use-visible'

const rules = {
  teamId: [{ required: true, message: '请选择班组', trigger: 'blur' }]
}
const formRef = ref()
const emit = defineEmits(['success', 'update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    required: true
  },
  details: {
    type: Object,
    default: () => {}
  }
})

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })
const submitLoading = ref(false)
const form = reactive({ quantity: null })

const unInProductionQuantity = computed(() => {
  return (props.details?.sourceSchedulingQuantity || 0) - (props.details?.inProductionQuantity || 0)
})

watch(
  () => props.visible,
  (visible) => {
    if (visible) {
      fetchTeamList()
      form.quantity = null
    }
  },
  { immediate: true }
)

const teamListLoading = ref(false)
const lines = ref([])
const linesMap = reactive({})
const taskId = ref()
async function fetchTeamList() {
  try {
    teamListLoading.value = true
    const { id, productionLineList } = await teamList({
      productType: props.details?.productType,
      schedulingId: props.details?.id
    })
    productionLineList.forEach((v) => {
      v.teamList = v.teamList.map((o) => {
        o.name = o.processName + ' - ' + o.leaderName
        return o
      })
      linesMap[v.id] = v
    })
    lines.value = productionLineList.filter(v => v.teamList.length)
    taskId.value = id
  } catch (error) {
    console.log('获取可协同生产线-班组列表', error)
  } finally {
    teamListLoading.value = false
  }
}

function submitIt() {
  formRef.value.validate(async (valid) => {
    if (valid) {
      const assistList = [{
        taskId: taskId.value,
        teamId: form.teamId
      }]
      await add({
        assistList
      })
      ElNotification({ title: '添加协同任务成功', type: 'success', duration: 2500 })
      emit('success')
      handleClose()
    } else {
      return false
    }
  })
}
</script>
