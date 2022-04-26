<template>
  <common-dialog
    title="添加备料清单"
    v-model="dialogVisible"
    width="850px"
    show-close
    :before-close="handleClose"
    custom-class="plan-material-preparation-project-form"
    top="10vh"
  >
    <template #titleRight>
      <common-button size="mini" type="success" icon="el-icon-plus" @click="addRow(addList)" />
      <common-button :loading="submitLoading" type="primary" size="mini" @click="submit">保 存</common-button>
    </template>
    <common-table
      :data="addList"
      :show-empty-symbol="false"
      return-source-data
      :cell-class-name="wrongCellMask"
      :max-height="maxHeight"
      row-key="uid"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column label="钢材种类" key="steelClassifyConfId" prop="steelClassifyConfId" align="center" width="150">
        <template #default="{ row, $index }">
          <part-steel-classify-type-select v-model="row.steelClassifyConfId" :show-extra="$index !== 0" />
        </template>
      </el-table-column>
      <el-table-column label="材质" key="material" prop="material" align="center" width="120">
        <template #default="{ row }">
          <el-input v-model="row.material" placeholder="材质" />
        </template>
      </el-table-column>
      <el-table-column label="厚度/规格" key="specification" prop="specification" align="center" min-width="120">
        <template #default="{ row }">
          <el-input v-model="row.specification" placeholder="厚度/规格" />
        </template>
      </el-table-column>
      <el-table-column label="清单量(kg)" key="listMete" prop="listMete" align="center" width="150">
        <template #default="{ row }">
          <el-input-number v-model="row.listMete" :controls="false" :max="999999999" placeholder="清单量" />
        </template>
      </el-table-column>
      <el-table-column label="操作" align="center" width="70">
        <template #default="{ $index }">
          <common-button type="danger" icon="el-icon-delete" size="mini" style="padding: 6px" @click.stop="removeRow(addList, $index)" />
        </template>
      </el-table-column>
    </common-table>
  </common-dialog>
</template>

<script setup>
import { defineProps, defineEmits, ref } from 'vue'
import { deepClone } from '@/utils/data-type'
import { positiveNumPattern } from '@/utils/validate/pattern'
import { operationTypeEnum } from '@/utils/enum/modules/common'
import { createUniqueString } from '@/utils/data-type/string'

import useVisible from '@/composables/use-visible'
import useTableOperate from '@/composables/form/use-table-operate'
import useMaxHeight from '@/composables/use-max-height'
import useTableValidate from '@/composables/form/use-table-validate'
import useSteelClassifyConf from '@/composables/store/use-steel-material-classify'
import partSteelClassifyTypeSelect from '@/components-system/base/part-steel-classify-type-select.vue'
import { ElMessage, ElNotification } from 'element-plus'

const emit = defineEmits(['update:modelValue', 'success'])

const props = defineProps({
  modelValue: {
    type: Boolean,
    default: false
  },
  technologyList: {
    type: Array,
    default: () => []
  }
})

// 添加列表
const addList = ref([])
// 提交loading
const submitLoading = ref(false)

// 同上的选项与值
const ditto = new Map([['steelClassifyConfId', -1]])

// 表格操作
const defaultRow = {
  steelClassifyConfId: undefined, // 钢材种类id
  material: undefined, // 材质
  specification: undefined, // 厚度/规格
  listMete: undefined // 清单量
}
const { init, addRow, removeRow } = useTableOperate(defaultRow, 10, ditto)

// dlg显示
const { visible: dialogVisible, handleClose } = useVisible({
  emit,
  props,
  showHook: () => {
    init(addList.value)
  }
})

// 表格校验
const tableRules = {
  steelClassifyConfId: [{ required: true, message: '请选择钢材种类', trigger: 'change' }],
  material: [{ required: true, max: 20, message: '请填写材质', trigger: 'blur' }],
  specification: [{ required: true, max: 60, message: '请填写厚度/规格', trigger: 'blur' }],
  listMete: [
    { required: true, max: 60, message: '请填写清单量', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '清单量必须大于0', trigger: 'blur' }
  ]
  // steelClassifyConfId: [{ required: true, max: 20, message: '不能超过50个字符', trigger: 'blur' }]
}
const { tableValidate, cleanUpData, wrongCellMask } = useTableValidate({ rules: tableRules, ditto })
const { steelClassifyConfKV } = useSteelClassifyConf()

// 添加
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.plan-material-preparation-project-form',
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    extraHeight: '10vh',
    navbar: false
  },
  dialogVisible
)

// 提交
async function submit() {
  try {
    submitLoading.value = true
    const { validResult, dealList } = tableValidate(addList.value)
    addList.value = dealList

    // 校验是否有重复的清单信息
    if (!validResult) return false
    // 清除无用数据
    const _list = cleanUpData(deepClone(dealList))
    _list.forEach((row) => {
      const steelClassifyConf = steelClassifyConfKV.value[row.steelClassifyConfId]
      row.id = createUniqueString()
      row.operateType = operationTypeEnum.ADD.V // 添加操作
      row.steelClassifyConfName = steelClassifyConf.name // 钢材配置名称
      row.basicClass = steelClassifyConf.basicClass // 主科目分类
      row.boundInvIds = [] // 绑定库存利用清单
      row.boundPurIds = [] // 绑定需要采购清单
    })
    const validateRepeatResult = validateRepeat(_list, props.technologyList)
    if (!validateRepeatResult) return false
    emit('success', _list)
    ElMessage.success('添加成功')
    handleClose()
  } catch (error) {
    console.log('科目添加', error)
  } finally {
    submitLoading.value = false
  }
}

// 验证是否有重复数据
function validateRepeat(addList = [], technologyList = []) {
  // 校验自身是否有重复数据
  let repeatMap = new Map()
  let repeatResult = []
  // 遍历添加列表
  for (let i = 0; i < addList.length; i++) {
    // 若在重复集合中，则不需要再遍历
    if (!repeatMap.get(i)) {
      const currentRow = addList[i]
      const repeatIndexArr = [i + 1]
      for (let k = i + 1; k < addList.length; k++) {
        const compareRow = addList[k]
        // 当钢材种类、材质、规格/厚度 这几样都相同时，视为重复
        if (
          currentRow.steelClassifyConfId === compareRow.steelClassifyConfId &&
          currentRow.material === compareRow.material &&
          currentRow.specification === compareRow.specification
        ) {
          repeatMap.set(k, true)
          repeatIndexArr.push(k + 1)
        }
      }
      if (repeatIndexArr.length > 1) {
        repeatResult.push({
          info: currentRow,
          index: repeatIndexArr
        })
      }
    }
  }
  if (repeatResult.length > 0) {
    let msg = ''
    repeatResult.forEach((item, index) => {
      msg += `${index + 1}.${item.info.steelClassifyConfName}-${item.info.material}-${item.info.specification}: 第${item.index.join(
        '、'
      )}行\n`
    })
    ElNotification.warning({ title: '添加清单数据重复', message: msg, duration: 60000 })
    return false
  }

  // 校验与当前已有备料单是否冲突
  // 初始化值
  repeatMap = new Map()
  repeatResult = []
  // 遍历添加列表
  for (let i = 0; i < addList.length; i++) {
    const currentRow = addList[i]
    for (let k = 0; k < technologyList.length; k++) {
      const compareRow = technologyList[k]
      if (
        currentRow.steelClassifyConfId === compareRow.steelClassifyConfId &&
        currentRow.material === compareRow.material &&
        currentRow.specification === compareRow.specification
      ) {
        repeatResult.push({
          info: currentRow,
          index: i + 1
        })
        break
      }
    }
  }
  if (repeatResult.length > 0) {
    let msg = ''
    repeatResult.forEach((item, index) => {
      msg += `${index + 1}.${item.info.steelClassifyConfName}-${item.info.material}-${item.info.specification}: 第${item.index}行\n`
    })
    ElNotification.warning({ title: '添加清单与现有清单数据重复', message: msg, duration: 60000 })
    return false
  }
  return true
}
</script>
