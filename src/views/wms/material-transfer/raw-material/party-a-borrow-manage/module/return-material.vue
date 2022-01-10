<template>
  <common-dialog
    title="归还办理"
    v-model="dialogVisible"
    width="1800px"
    :before-close="handleClose"
    :show-close="true"
    custom-class="wms-borrow-return-handling"
    top="10vh"
  >
    <template #titleRight>
      <common-button :loading="submitLoading" :disabled="!returnedTotalQuantity" size="mini" type="primary" @click="submit">
        提 交
      </common-button>
    </template>
    <el-form ref="formRef" class="form" :model="form" :rules="rules" size="small" label-position="right" inline label-width="70px">
      <div class="form-header">
        <el-form-item label="项目" prop="projectId">
          <span v-parse-project="{ project: detail.project }" v-empty-text />
        </el-form-item>
        <el-form-item label="工厂" prop="factoryId">
          <factory-select v-model="form.factoryId" placeholder="工厂" class="input-underline" style="width: 200px" />
        </el-form-item>
        <el-form-item label="仓库" prop="warehouseId">
          <warehouse-select
            v-model="form.warehouseId"
            :factory-id="form.factoryId"
            :basic-class="basicClass"
            placeholder="存储位置"
            class="input-underline"
            style="width: 200px"
          />
        </el-form-item>
        <br />
        <div class="flex-rbc">
          <el-form-item label="类型">
            <common-radio-button
              type="enum"
              v-model="filterParams.type"
              :options="projectWareTypeEnum"
              :disabled-val="projectWareTypeDisabled"
              show-option-all
              clearable
            />
          </el-form-item>
          <div>
            <el-form-item label="只看填写归还数量的材料" label-width="170px">
              <el-checkbox v-model="filterParams.hasReturnedQuantity" size="mini"></el-checkbox>
            </el-form-item>
            <el-form-item label="本次归还统计" label-width="100px">
              <span class="returned-number">{{ returnedTotalQuantity }}</span>
              <span>{{ ` / ${detail.corPendingQuantity} ${detail.outboundUnit}` }}</span>
            </el-form-item>
            <el-form-item label="审核中的数量" label-width="100px">
              <span>{{ `${detail.corUnderReviewQuantity} ${detail.outboundUnit}` }}</span>
            </el-form-item>
          </div>
        </div>
      </div>
      <common-table
        ref="tableRef"
        v-loading="matListLoading"
        :data="filterMatList"
        :max-height="maxHeight"
        :default-expand-all="false"
        :expand-row-keys="expandRowKeys"
        row-key="id"
      >
        <el-expand-table-column :data="filterMatList" v-model:expand-row-keys="expandRowKeys" row-key="id" fixed="left">
          <template #default="{ row }">
            <expand-secondary-info :basic-class="row.basicClass" :row="row">
              <p>项目：<span v-if="row.project" v-parse-project="{ project: row.project }" v-empty-text /></p>
              <el-input
                v-model="row.remark"
                :autosize="remarkTextSize"
                type="textarea"
                placeholder="备注"
                maxlength="200"
                show-word-limit
                style="max-width: 400px"
              />
            </expand-secondary-info>
          </template>
        </el-expand-table-column>
        <!-- 基础信息 -->
        <material-base-info-columns :basic-class="basicClass" fixed="left" />
        <!-- 单位及其数量 -->
        <material-unit-operate-quantity-columns :basic-class="basicClass" />
        <!-- 次要信息 -->
        <material-secondary-info-columns :basic-class="basicClass" :show-batch-no="false" />
        <warehouse-info-columns show-project />
        <el-table-column label="归还数量" width="170px" align="center" fixed="right">
          <template #default="{ row }">
            <span class="flex-rbc">
              <common-input-number
                v-model="row.returnedQuantity"
                :min="0"
                :precision="row.outboundUnitPrecision"
                :max="row.operableQuantity"
                controls-position="right"
                @change="handleQuantityChange($event, row)"
              />
              <span style="flex: none; margin-left: 10px">{{ row.outboundUnit }}</span>
            </span>
          </template>
        </el-table-column>
      </common-table>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { returnMaterial, getReturnableMatListById } from '@/api/wms/material-transfer/raw-material/party-a-borrow-manage'
import { defineEmits, defineProps, ref, watch, computed } from 'vue'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'

import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'
import useWmsConfig from '@/composables/store/use-wms-config'
import FactorySelect from '@/components-system/base/factory-select.vue'
import WarehouseSelect from '@/components-system/wms/warehouse-select.vue'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import expandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'
import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import materialUnitOperateQuantityColumns from '@/components-system/wms/table-columns/material-unit-operate-quantity-columns/index.vue'
import materialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import warehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import { measureTypeEnum } from '@/utils/enum/modules/wms'
import { ElMessage } from 'element-plus'

const emit = defineEmits(['success', 'update:visible'])

const props = defineProps({
  visible: {
    type: Boolean,
    require: true
  },
  basicClass: {
    // 基础分类
    type: Number
  },
  detail: {
    // 物料归还信息
    type: Object,
    default: () => {
      return {}
    }
  }
})

// 仓库类型
const projectWareTypeEnum = {
  PUBLIC: { L: '公共库', K: 'PUBLIC', V: 1 },
  CUR_PROJECT: { L: '当前项目', K: 'CUR_PROJECT', V: 2 },
  OTHER_PROJECT: { L: '其他项目', K: 'OTHER_PROJECT', V: 3 }
}

// 校验
const rules = {
  factoryId: [{ required: true, message: '请选择归还工厂', trigger: 'change' }],
  warehouseId: [{ required: true, message: '请选择归还仓库', trigger: 'change' }]
}

// 表单ref
const formRef = ref()
// 表格展开key列表
const expandRowKeys = ref([])
// 可归还的材料列表
const returnableMatList = ref([])
// 归还列表
const returnList = ref([])
// 可归还的材料列表加载状态
const matListLoading = ref(false)
// 过滤列表参数
const filterParams = ref({
  type: projectWareTypeEnum.CUR_PROJECT.V,
  hasReturnedQuantity: false
})
// 禁用类型
const projectWareTypeDisabled = ref([])
// 提交表单
const form = ref({
  list: []
})
// 提交loading
const submitLoading = ref(false)
// 显示
const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: clearValidate })
//
const { partyABorrowReturnCfg } = useWmsConfig()

watch(
  partyABorrowReturnCfg,
  () => {
    if (partyABorrowReturnCfg.value && partyABorrowReturnCfg.value.boolReturnByOtherProject) {
      projectWareTypeDisabled.value = []
    } else {
      projectWareTypeDisabled.value = [projectWareTypeEnum.OTHER_PROJECT.V]
    }
  },
  { immediate: true, deep: true }
)
// 表格最大高度
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.wms-borrow-return-handling',
    extraBox: ['.el-dialog__header', '.form-header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    navbar: false,
    minHeight: 350
  },
  dialogVisible
)
// 基础类型
const basicClass = computed(() => props.detail.basicClass)

// 筛选后的材料列表
const filterMatList = computed(() => {
  const list = filterParams.value.hasReturnedQuantity ? returnList.value : returnableMatList.value
  if (!filterParams.value.type) return list
  if (filterParams.value.type === projectWareTypeEnum.PUBLIC.V) {
    return list.filter((row) => !row.project || !row.project.id)
  }
  if (filterParams.value.type === projectWareTypeEnum.CUR_PROJECT.V) {
    return list.filter((row) => {
      // 与当前借用项目相同
      const flag = row.project && props.detail.borrowProject && row.project.id === props.detail.borrowProject.id
      return flag
    })
  }
  if (filterParams.value.type === projectWareTypeEnum.OTHER_PROJECT.V) {
    return list.filter((row) => {
      // 与当前借用项目不相同
      const flag = row.project && props.detail.borrowProject && row.project.id && row.project.id !== props.detail.borrowProject.id
      return flag
    })
  }
  return list
})

// 归还总数
const returnedTotalQuantity = computed(() => {
  return returnList.value.reduce((sum, cur) => {
    return sum + (cur.returnedQuantity || 0)
  }, 0)
})

// 备注输入框大小
const remarkTextSize = computed(() => {
  if (props.basicClass === matClsEnum.STEEL_PLATE.V) {
    return { minRows: 2, maxRows: 2 }
  }
  return { minRows: 1, maxRows: 1 }
})

watch(
  () => props.detail,
  (detail) => {
    // 初始化并查询列表
    init()
    fetchReturnableMatList(detail.id)
  },
  { immediate: true }
)

// 初始化
function init() {
  form.value = {
    list: []
  }
  formRef.value && formRef.value.resetFields()
  returnableMatList.value = []
  returnList.value = []
  expandRowKeys.value = []
  filterParams.value.type = projectWareTypeEnum.CUR_PROJECT.V
  filterParams.value.hasReturnedQuantity = false
}

// 可归还的物料列表
async function fetchReturnableMatList(id) {
  returnableMatList.value = []
  if (!id) return
  try {
    matListLoading.value = true
    const { content = [] } = await getReturnableMatListById(id)
    await setSpecInfoToList(content)
    returnableMatList.value = await numFmtByBasicClass(content, {
      toSmallest: false,
      toNum: false
    })
    returnableMatList.value.forEach((v) => {
      v.operableQuantity = v.quantity - v.frozenQuantity
      v.operableMete = v.mete - v.frozenMete
      if (v.outboundUnitType === measureTypeEnum.MEASURE.V) {
        // 实际在出库中使用的数量
        v.corQuantity = v.quantity // 数量
        v.corFrozenQuantity = v.frozenQuantity // 冻结数量
        v.corOperableQuantity = v.operableQuantity // 可操作数量
      } else {
        // 核算量
        v.corQuantity = v.mete
        v.corFrozenQuantity = v.frozenMete
        v.corOperableQuantity = v.operableMete
      }
    })
  } catch (error) {
    console.log('加载可归还物料列表', error)
  } finally {
    matListLoading.value = false
  }
}

// 处理数量变化
function handleQuantityChange(val, row) {
  const index = returnList.value.findIndex((item) => item.id === row.id)
  // 当val为0或空，且row在归还列表中时，移除归还列表
  if (index > -1 && !val) {
    returnList.value.splice(index, 1)
  }
  // 当val有值，且row不在归还列表中时，加入归还列表
  if (index === -1 && val) {
    returnList.value.push(row)
  }
}

// 关闭回调
function clearValidate() {
  formRef.value && formRef.value.clearValidate()
}

// 批量归还提交
async function submit() {
  try {
    submitLoading.value = true
    const valid = await formRef.value.validate()
    if (!valid) return
    // 数据格式装换
    const data = {
      id: props.detail.id, // 当前归还id
      factoryId: form.value.factoryId, // 工厂id
      warehouseId: form.value.warehouseId, // 仓库id
      list: returnList.value.map((row) => {
        return {
          id: row.id,
          quantity: row.returnedQuantity,
          outboundUnit: row.outboundUnit,
          outboundUnitType: row.outboundUnitType
        }
      })
    }
    if (data.list.length === 0) {
      ElMessage.warning('请填写数据')
      return
    }
    await returnMaterial(data)
    emit('success')
    handleClose()
    setTimeout(() => {
      init()
    }, 0)
  } catch (error) {
    console.log('归还办理', error)
  } finally {
    submitLoading.value = false
  }
}
</script>

<style lang="scss" scoped>
.el-form-item {
  align-items: center;
  ::v-deep(.el-form-item__content) {
    line-height: unset;
  }
}
.returned-number {
  color: green;
}
</style>
