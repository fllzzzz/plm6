<template>
  <common-dialog ref="dialogRef" title="套料设定" v-model:visible="dialogVisible" direction="rtl" :before-close="handleClose" width="400px">
    <template #titleRight>
      <common-button v-loading.fullscreen.lock="fullscreenLoading" type="success" size="mini" @click="submitForm(formRef)">
        开始套料
      </common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="130px" class="demo-form">
      <el-form-item label="预留割缝≤" prop="kerfLength">
        <el-input v-model="form.kerfLength" placeholder="输入长度 单位：mm" />
      </el-form-item>
      <el-form-item label="母材长度≤" prop="length">
        <el-input v-model="form.length" placeholder="输入长度 单位：mm" />
      </el-form-item>
      <el-form-item label="套料方式" prop="typesettingTypeEnum">
        <template #label>
          套料方式
          <el-tooltip
            effect="dark"
            :content="`如焊接型部件，可选择无损套料，选择无损套料将会按照约定的长度内进行套料\n
              如型材部件，可选择无损套料，选择有损套料，可能会产生一定的材料损耗`"
            placement="bottom"
          >
            <i class="el-icon-info" />
          </el-tooltip>
        </template>
        <common-radio class="filter-item" v-model="form.typesettingTypeEnum" :options="typeSettingTypeEnum.ENUM" type="enum" size="small" />
      </el-form-item>
      <!-- <el-form-item>
        <common-button
            v-loading.fullscreen.lock="fullscreenLoading"
            type="success"
            size="small"
            @click="submitForm(formRef)"
          >开始套料</common-button
        >
      </el-form-item> -->
    </el-form>
  </common-dialog>
  <nesting-progress v-model="extrusionVisible" :batchId="batchId" />
</template>

<script setup>
import { defineProps, ref, defineEmits, reactive } from 'vue'
import { typeSettingTypeEnum } from '@enum-ms/mes'
import useVisible from '@compos/use-visible'
import { extrusionNesting } from '@/api/mes/craft-manage/section-steel/nesting-setting'
// import nestingProgress from './nesting-progress.vue'

const formRef = ref()
const dialogRef = ref()
const fullscreenLoading = ref(false)
const extrusionVisible = ref(false)
const batchId = ref()
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  projectId: {
    type: [String, Number],
    default: undefined
  },
  detailData: {
    type: Array,
    default: () => []
  }
})
const emit = defineEmits(['update:visible', 'refresh'])
const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

const form = reactive({
  kerfLength: 2, // 预留割缝
  length: 12000, // 母材长度
  typesettingTypeEnum: undefined // 套料方式
})
const rules = {
  kerfLength: [{ required: true, message: '请输入预留割缝', trigger: 'blur' }],
  length: [{ required: true, message: '请输入母材长度', trigger: 'blur' }],
  typesettingTypeEnum: [{ required: true, message: '请选择套料方式', trigger: 'blur' }]
}

async function submitForm(formRef) {
  try {
    const _data = []
    props.detailData.map((v) => {
      _data.push({
        id: v.id,
        quantity: v.quantity
      })
    })
    const _list = {
      assembleSettingList: _data,
      projectId: props.projectId,
      length: form.length,
      kerfLength: form.kerfLength,
      typesettingTypeEnum: form.typesettingTypeEnum
    }
    await extrusionNesting(_list)
    handleClose()
    fullscreenLoading.value = true
    setTimeout(() => {
      fullscreenLoading.value = false
    }, 2000)
    console.log(props.detailData, 'props.detailData')
    extrusionVisible.value = true
  } catch (err) {
    console.log('套料失败')
  }
}
</script>
