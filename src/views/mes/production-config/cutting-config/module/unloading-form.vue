<template>
  <!-- 下料方式配置 -->
  <common-dialog
    title="下料方式配置"
    v-model="visible"
    width="600px"
    :before-close="handleClose"
    :show-close="true"
    :close-on-click-modal="false"
    top="10vh"
  >
    <template #titleRight>
      <common-button size="mini" type="success" icon="el-icon-plus" @click="addRow(form.list)" />
      <common-button :loading="submitLoading" type="primary" size="mini" @click="submit">提 交</common-button>
    </template>
    <div>
      <common-radio-button
        style="margin-bottom: 8px"
        class="filter-item"
        v-model="materialType"
        :options="materialTypeEnum.ENUM"
        type="enum"
        size="small"
      />
      <el-form ref="formRef" :model="form" :disabled="submitLoading">
        <common-table
          :data="form.list"
          :show-empty-symbol="false"
          return-source-data
          empty-text="暂无数据"
          :max-height="maxHeight"
          default-expand-all
          :cell-class-name="wrongCellMask"
          style="width: 100%"
        >
          <el-table-column label="序号" type="index" align="center" width="60" />
          <el-table-column key="layingOffWay" prop="layingOffWay" label="下料方式" align="center" min-width="180">
            <template #default="{ row }">
              <el-input v-model="row.layingOffWay" placeholder="下料方式" style="width: 100%" />
            </template>
          </el-table-column>
          <el-table-column key="taskPrefix" prop="taskPrefix" label="任务前缀" align="center" min-width="180">
            <template #default="{ row }">
              <el-input v-model="row.taskPrefix" placeholder="任务前缀" style="width: 100%" />
            </template>
          </el-table-column>
          <el-table-column label="操作" width="70px" align="center">
            <template #default="{ $index }">
              <common-button
                type="danger"
                icon="el-icon-delete"
                size="mini"
                style="padding: 6px"
                @click.stop="removeRow(form.list, $index)"
              />
            </template>
          </el-table-column>
        </common-table>
      </el-form>
    </div>
  </common-dialog>
</template>

<script setup>
import { ref, defineEmits, defineProps, watch, reactive } from 'vue'
import { batchUnloadingAdd } from '@/api/mes/production-config/unloading-config'
import { materialTypeEnum } from '@enum-ms/uploading-form'
import { deepClone } from '@data-type/index'
import useVisible from '@compos/use-visible'
import useTableOperate from '@compos/form/use-table-operate'
import useTableValidate from '@compos/form/use-table-validate'
import useAddFormLocalStorage from '@compos/form/use-add-form-local-storage'
import useMaxHeight from '@compos/use-max-height'
import { ElMessage } from 'element-plus'

const tableRules = {
  layingOffWay: [{ required: true, message: '请输入下料方式', trigger: 'blur' }],
  taskPrefix: [{ require: true, message: '请输入任务前缀', trigger: 'blur' }]
}

const form = reactive({ list: [] })

const defaultRow = {}
const formRef = ref()
const originList = ref()
const submitLoading = ref(false)
const materialType = ref(materialTypeEnum.MANMADE_BLANKING.V)
const emit = defineEmits(['success', 'update:modelValue'])
const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  detailData: {
    type: Array,
    default: () => []
  }
})

const { visible, handleClose } = useVisible({ emit, props })

const { init, addRow, removeRow } = useTableOperate(defaultRow, 1)
const { tableValidate, cleanUpData, wrongCellMask } = useTableValidate({ rules: tableRules })
const { ADD_FORM, clearFormStorage } = useAddFormLocalStorage(form, visible)

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.mes-cutting-config',
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    navbar: false
  },
  visible
)

// 表格初始化
ADD_FORM.init = () => init(form.list)

watch(
  () => visible.value,
  (val) => {
    if (val) {
      originList.value = JSON.parse(JSON.stringify(props.detailData)) || []
      form.list = originList.value.filter((v) => {
        return v.materialType === materialType.value
      })
    }
  },
  { immediate: true }
)

watch(
  () => materialType.value,
  (val) => {
    if (val) {
      form.list = originList.value.filter((v) => {
        return v.materialType === materialType.value
      })
    }
  }
)

// 提交表单
async function submit() {
  try {
    submitLoading.value = true
    const { validResult, dealList } = tableValidate(form.list)
    form.list = dealList
    if (validResult) {
      // 清除无用数据
      form.list = props.detailData
      const _list = cleanUpData(deepClone(dealList))

      const submitData = []
      _list.map((v) => {
        submitData.push({
          layingOffWay: v.layingOffWay,
          taskPrefix: v.taskPrefix.toUpperCase()
        })
      })
      // 数据格式化
      await batchUnloadingAdd({
        materialType: materialType.value,
        mesBuildingLayWayDTOParams: submitData
      })
      // 清除本地缓存
      clearFormStorage()
      emit('success')
      ElMessage.success('添加成功')
      handleClose()
    }
  } catch (error) {
    console.log('下料方式配置', error)
  } finally {
    submitLoading.value = false
  }
}

// 表单提交数据清理
// crud.submitBatchFormFormat = (form) => cleanUpData(form.list)
</script>
