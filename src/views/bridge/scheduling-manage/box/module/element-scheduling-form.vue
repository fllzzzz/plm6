<template>
  <common-drawer
    ref="elementDrawerRef"
    modalClass="element-scheduling-drawer"
    title="单元件排产"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="100%"
  >
    <template #titleRight>
      <common-button size="mini" type="success" @click="handleClose"> 上一步【分段排产预览】 </common-button>
      <common-button v-permission="permission.elementSave" size="mini" :loading="taskLoading" type="primary" @click="toTaskIssue">
        任务下发
      </common-button>
    </template>
    <template #content>
      <div class="head-container">
        <!-- <span class="filter-item">分段排产信息：</span> -->
        <tag-tabs v-model="curGroupsId" class="filter-item" :style="'width:calc(100% - 125px)'" :data="showTagList" itemKey="groupsId">
          <template #default="{ item }">
            <span>{{ item.label }}</span>
            <template v-if="item.groupsId !== paGroupId">
              <span> - </span>
              <span>排产量：</span>
              <span>{{ item.mergeQuantity }}件 / {{ item.mergeWeight.toFixed(2) }}kg</span>
            </template>
          </template>
        </tag-tabs>
      </div>
      <!-- <div class="tip">
        <span>* 提示：</span>
        <span> 系统自动默认与分段相同的产线进行单元件生产，也可在生产组列修改产线或生产组。</span>
      </div> -->
      <common-table
        v-loading="tableLoading"
        :data="tableData"
        :max-height="otherData.length ? maxHeight / 2 : maxHeight"
        :cell-class-name="wrongCellMask"
        :stripe="false"
        style="width: 100%"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="attributeType" :show-overflow-tooltip="true" label="属性" width="90" align="center">
          <template #default="{ row }">
            <el-tag :type="row.attributeType === '单元件' ? 'warning' : 'success'">{{ row.attributeType }}</el-tag>
          </template>
        </el-table-column>
        <!-- <el-table-column prop="typesettingAssembleTypeEnum" :show-overflow-tooltip="true" label="单元件类型" min-width="100" align="center">
          <template #default="{ row: { sourceRow: row } }">
            <span>{{
              row.typesettingAssembleTypeEnum ? mesBuildingTypeSettingAssembleTypeEnum.VL[row.typesettingAssembleTypeEnum] : '-'
            }}</span>
          </template>
        </el-table-column> -->
        <el-table-column prop="elementConfigName" :show-overflow-tooltip="true" label="单元件类型" min-width="100" align="center"/>
        <el-table-column prop="serialNumber" :show-overflow-tooltip="true" label="编号" min-width="100" align="center">
          <template #default="{ row }">
            <span>{{ row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="specification" :show-overflow-tooltip="true" label="规格" min-width="120" align="center" />
        <el-table-column prop="commonLength" :show-overflow-tooltip="true" label="长度（mm）" min-width="90" align="center" />
        <el-table-column prop="weight" :show-overflow-tooltip="true" label="重量（kg）" min-width="90" align="center" />
        <el-table-column prop="needSchedulingQuantity" :show-overflow-tooltip="true" label="数量" min-width="90" align="center">
          <template #default="{ row: { sourceRow: row } }">
            <!-- <el-input-number
              v-model="row.needSchedulingQuantity"
              :step="1"
              :min="0"
              :max="row.unSchedulingQuantity"
              :precision="0"
              size="mini"
              controls-position="right"
              style="width: 100%"
            /> -->
            <span>{{ row.needSchedulingQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="groupsId" label="生产组" min-width="150px" align="center">
          <template #default="{ row: { sourceRow: row }, $index }">
            <el-cascader
              v-model="row.groupsId"
              :options="classIdGroupsObj[row.elementConfigId]?.list"
              :props="{ value: 'id', label: 'name', children: 'children', expandTrigger: 'hover', emitPath: false }"
              filterable
              clearable
              style="width: 100%"
              :placeholder="$index === 0 ? '请选择生产组' : '同上'"
              @change="handleGroupsChange($event, row, $index)"
            />
          </template>
        </el-table-column>
        <el-table-column prop="askCompleteTime" label="要求完成日期" align="center" min-width="130px">
          <template #default="{ row: { sourceRow: row }, $index }">
            <el-date-picker
              v-model="row.askCompleteTime"
              type="date"
              size="mini"
              value-format="x"
              style="width: 100%"
              :disabledDate="(v) => moment(v).valueOf() < moment().subtract(1, 'days').valueOf()"
              :placeholder="$index === 0 ? '需求完成日期' : '同上'"
              @change="handleAskCompleteTimeChange($event, row, $index)"
            />
          </template>
        </el-table-column>
      </common-table>
      <el-divider v-if="otherData.length" class="element-scheduling-divider"><span class="title">型材</span></el-divider>
      <common-table
        v-if="otherData.length"
        v-loading="tableLoading"
        :data="otherData"
        :max-height="maxHeight / 2"
        :cell-class-name="wrongCellMask"
        :stripe="false"
        style="width: 100%; margin-top: 15px"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="attributeType" :show-overflow-tooltip="true" label="属性" width="90" align="center">
          <template #default="{ row }">
            <el-tag :type="row.attributeType === '单元件' ? 'warning' : 'success'">{{ row.attributeType }}</el-tag>
          </template>
        </el-table-column>
        <!-- <el-table-column prop="typesettingAssembleTypeEnum" :show-overflow-tooltip="true" label="单元件类型" min-width="100" align="center">
          <template #default="{ row: { sourceRow: row } }">
            <span>{{
              row.typesettingAssembleTypeEnum ? mesBuildingTypeSettingAssembleTypeEnum.VL[row.typesettingAssembleTypeEnum] : '-'
            }}</span>
          </template>
        </el-table-column> -->
        <el-table-column prop="elementConfigName" :show-overflow-tooltip="true" label="单元件类型" min-width="100" align="center"/>
        <el-table-column prop="serialNumber" :show-overflow-tooltip="true" label="编号" min-width="100" align="center" />
        <el-table-column prop="specification" :show-overflow-tooltip="true" label="规格" min-width="120" align="center" />
        <el-table-column prop="commonLength" :show-overflow-tooltip="true" label="长度（mm）" min-width="90" align="center" />
        <el-table-column prop="weight" :show-overflow-tooltip="true" label="重量（kg）" min-width="90" align="center" />
        <el-table-column prop="quantity" :show-overflow-tooltip="true" label="数量" min-width="90" align="center" />
      </common-table>
      <handle-surplus-element-dialog
        ref="handleSurplusRef"
        v-model:visible="surplusElementVisible"
        :surplusList="surplusElementList"
        :groupsId="props.boxList[0]?.groups?.id"
      >
        <template #saveBtn>
          <common-button size="mini" :loading="saveSurplusLoading" type="primary" @click="toSaveHandleSurplus"> 保存并下发 </common-button>
        </template>
      </handle-surplus-element-dialog>
    </template>
  </common-drawer>
</template>

<script setup>
import { getElement } from '@/api/bridge/scheduling-manage/box'
import { saveTask } from '@/api/bridge/scheduling-manage/common'
import { defineProps, defineEmits, ref, inject, reactive, computed, nextTick, watch } from 'vue'
import moment from 'moment'
import { ElNotification } from 'element-plus'

import { artifactProductLineEnum, mesBuildingTypeSettingAssembleTypeEnum } from '@enum-ms/mes'
import { bridgeComponentTypeEnum } from '@enum-ms/bridge'
import { isBlank, deepClone } from '@/utils/data-type'
import { obj2arr } from '@/utils/convert/type'
import { boxSchedulingPM as permission } from '@/page-permission/bridge'

import useTableValidate from '@compos/form/use-table-validate'
import { manualFetchGroupsTree } from '@compos/bridge/scheduling/use-scheduling-groups'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import tagTabs from '@comp-common/tag-tabs'
import handleSurplusElementDialog from './handle-surplus-element-dialog'

const productType = bridgeComponentTypeEnum.CELL.V

const elementDrawerRef = ref()
const emit = defineEmits(['update:visible', 'task-issue-success'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  boxList: {
    type: Array,
    default: () => []
  },
  productionLineTypeEnum: {
    type: [Number, String]
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook })

const factoryIds = inject('curFactoryIds')
const classIdGroupsObj = reactive({}) // {单元件类型id：{list：groupsTree，obj：groupsObj}}

// 高度
const { fixMaxHeight, maxHeight } = useMaxHeight(
  {
    mainBox: '.element-scheduling-drawer',
    extraBox: ['.el-drawer__header', '.head-container', '.element-scheduling-divider'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true
  },
  elementDrawerRef
)

const paGroupId = -1 // 母件 默认一组 组id为-1
const tableLoading = ref()
const tagObj = ref({})
const tagList = ref([])
const showTagGroupIds = ref([])
const curGroupsId = ref()
const originElementSchedulingList = ref([])
const surplusElementList = ref([])
const surplusElementVisible = ref(false)

const tableData = computed(() => tagObj.value[curGroupsId.value]?.elementList || [])
const otherData = computed(() => tagObj.value[curGroupsId.value]?.otherList || [])
const hasOtherData = computed(() => Boolean(otherData.value?.length))
const showTagList = computed(() => {
  const _arr = []
  for (let i = 0; i < tagList.value.length; i++) {
    const v = tagList.value[i]
    if (showTagGroupIds.value.includes(v.groupsId)) {
      _arr.push(v)
    }
  }
  return _arr
})

watch(
  () => hasOtherData.value,
  () => {
    nextTick(() => {
      fixMaxHeight()
    })
  }
)

const tableRules = {
  needSchedulingQuantity: [{ required: true, message: '请填写数量', trigger: 'blur' }],
  groupsId: [{ required: true, message: '请选择生产组', trigger: 'change' }],
  askCompleteTime: [{ required: true, message: '请选择需求完成日期', trigger: 'change' }]
}
const ditto = new Map([
  ['groupsId', '同上'],
  ['askCompleteTime', '同上']
])
const { tableValidate, cleanUpData, wrongCellMask } = useTableValidate({ rules: tableRules, ditto })

function handleGroupsChange(val, row, index) {
  if (index !== 0 && !val) {
    row.groupsId = '同上'
  }
}

function handleAskCompleteTimeChange(val, row, index) {
  if (index !== 0 && !val) {
    row.askCompleteTime = '同上'
  }
}

function showHook() {
  initBoxData(props.boxList)
  fetch()
}

async function fetch() {
  // const ids = (curGroupsId.value && tagObj.value[curGroupsId.value]?.ids) || []
  // if (!ids || !ids.length) return
  try {
    tableLoading.value = true
    surplusElementList.value = []
    const _ids = props.boxList.map((v) => {
      return {
        id: v.id,
        quantity: v.schedulingQuantity
      }
    })
    const { elementSchedulingList, elementTypesetting, surplusElement } = await getElement(_ids)
    showTagGroupIds.value = []
    originElementSchedulingList.value = elementSchedulingList
    // 处理单元件信息
    for (let i = 0; i < elementSchedulingList?.length; i++) {
      const v = elementSchedulingList[i]
      if (v?.groupsId && tagObj.value[v.groupsId]) {
        showTagGroupIds.value.push(v.groupsId)
        tagObj.value[v.groupsId].elementList = []
        tagObj.value[v.groupsId].unshowList = []
        tagObj.value[v.groupsId].otherList = []
        for (let o = 0; o < v.elementList?.length; o++) {
          const _o = v.elementList[o]
          _o.boolStructuralEnum = false
          _o.attributeType = '单元件'
          _o.weight = _o.netWeight
          _o.commonLength = _o.length
          _o.needSchedulingQuantity = _o.quantity
          _o.boolTypesettinglEnum = false
          if (o !== 0) {
            _o.groupsId = '同上'
            _o.askCompleteTime = '同上'
          }
          if (!_o.boolProcess || (props.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V && _o.boolSectionSteel)) {
            tagObj.value[v.groupsId].unshowList.push({
              boolTypesettinglEnum: _o.boolTypesettinglEnum,
              boolSectionSteel: _o.boolSectionSteel,
              boolProcess: _o.boolProcess,
              productId: _o.productId,
              projectId: _o.projectId,
              quantity: _o.quantity,
              boolStructuralEnum: _o.boolStructuralEnum
            })
            tagObj.value[v.groupsId].otherList.push({ ..._o })
          } else {
            tagObj.value[v.groupsId].elementList.push({ ..._o })
          }

          if (isBlank(classIdGroupsObj[_o.elementConfigId])) {
            classIdGroupsObj[_o.elementConfigId] = await manualFetchGroupsTree({
              productType,
              structureClassId: _o.elementConfigId,
              _factoryIds: factoryIds.value
            })
          }
        }
        // 处理母件
        for (let x = 0; x < v.elementTypesetting?.length; x++) {
          const _o = elementTypesetting[x]
          _o.productId = _o.id
          _o.attributeType = '套料'
          _o.weight = _o.nestingNetWeight
          _o.commonLength = _o.typesettingLength
          _o.boolStructuralEnum = true
          _o.boolTypesettinglEnum = true
          _o.needSchedulingQuantity = 1
          if (x !== 0 || (v.elementList?.length && x === 0)) {
            _o.groupsId = '同上'
            _o.askCompleteTime = '同上'
          }
          if (_o.elementConfigId && isBlank(classIdGroupsObj[_o.elementConfigId])) {
            classIdGroupsObj[_o.elementConfigId] = await manualFetchGroupsTree({
              productType,
              structureClassId: _o.elementConfigId,
              _factoryIds: factoryIds.value
            })
          }
          if (
            (props.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V &&
              _o.typesettingAssembleTypeEnum === mesBuildingTypeSettingAssembleTypeEnum.FINISHED.V) ||
            (props.productionLineTypeEnum === artifactProductLineEnum.TRADITION.V && !_o.boolProcess)
          ) {
            tagObj.value[v.groupsId].unshowList.push({
              boolStructuralEnum: _o.boolStructuralEnum,
              boolTypesettinglEnum: _o.boolTypesettinglEnum,
              typesettingAssembleTypeEnum: _o.typesettingAssembleTypeEnum,
              boolProcess: _o.boolProcess,
              productId: _o.productId,
              projectId: _o.projectId,
              quantity: _o.quantity
            })
            tagObj.value[v.groupsId].otherList.push({ ..._o })
          } else {
            tagObj.value[v.groupsId].elementList.push({ ..._o })
          }
        }
      }
    }
    // 处理母件信息
    // if (elementTypesetting?.length) {
    //   const _list = []
    //   const _unshowList = []
    //   const _otherList = []
    //   for (let x = 0; x < elementTypesetting.length; x++) {
    //     const v = elementTypesetting[x]
    //     v.productId = v.id
    //     v.attributeType = '套料'
    //     v.boolStructuralEnum = true
    // v.weight = v.netWeight
    // v.commonLength = v.length
    //     v.boolTypesettinglEnum = true
    //     v.needSchedulingQuantity = 1
    //     if (x !== 0) {
    //       v.groupsId = '同上'
    //       v.askCompleteTime = '同上'
    //     }
    //     if (v.elementConfigId && isBlank(classIdGroupsObj[v.elementConfigId])) {
    //       classIdGroupsObj[v.elementConfigId] = await manualFetchGroupsTree({
    //         productType,
    //         structureClassId: v.elementConfigId,
    //         _factoryIds: factoryIds.value
    //       })
    //     }
    //     // _list.push(v)
    //     if (
    //       (props.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V &&
    //         v.typesettingAssembleTypeEnum === mesBuildingTypeSettingAssembleTypeEnum.FINISHED.V) ||
    //       (props.productionLineTypeEnum === artifactProductLineEnum.TRADITION.V && !v.boolProcess)
    //     ) {
    //       _unshowList.push({
    //         boolStructuralEnum: v.boolStructuralEnum,
    //         boolTypesettinglEnum: v.boolTypesettinglEnum,
    //         typesettingAssembleTypeEnum: v.typesettingAssembleTypeEnum,
    //         boolProcess: v.boolProcess,
    //         productId: v.productId,
    //         projectId: v.projectId,
    //         quantity: v.quantity
    //       })
    //       _otherList.push(v)
    //     } else {
    //       _list.push(v)
    //     }
    //   }
    //   const _obj = {
    //     label: '母件',
    //     mergeQuantity: 0,
    //     mergeWeight: 0,
    //     elementList: _list,
    //     unshowList: _unshowList,
    //     otherList: _otherList,
    //     ids: [],
    //     groupsId: paGroupId
    //   }
    //   tagObj.value[paGroupId] = _obj
    //   tagList.value.push(_obj)
    //   showTagGroupIds.value.push(paGroupId)
    // }
    if (props.productionLineTypeEnum & artifactProductLineEnum.INTELLECT.V) {
      surplusElementList.value = surplusElement || []
    }
    curGroupsId.value = showTagGroupIds.value[0]
  } catch (error) {
    console.log('获取单元件排产列表失败', error)
  } finally {
    tableLoading.value = false
    nextTick(() => {
      fixMaxHeight()
    })
  }
}

// 分段按生产组分类
function initBoxData(list) {
  const _list = list
  const _tagObj = {} // 分段信息对象{groupsId:{}}
  for (let i = 0; i < _list.length; i++) {
    const groupsId = _list[i].groups?.id
    if (isBlank(_tagObj[groupsId])) {
      _tagObj[groupsId] = {
        groupsId,
        label: `${_list[i].workshop?.name}>${_list[i].productionLine?.name}>${_list[i].groups?.name}`,
        mergeQuantity: 0,
        mergeWeight: 0,
        ids: [],
        list: []
      }
    }
    _tagObj[groupsId].mergeQuantity += _list[i].schedulingQuantity
    _tagObj[groupsId].mergeWeight += _list[i].schedulingTotalNetWeight
    _tagObj[groupsId].ids.push({
      id: _list[i].id,
      schedulingId: _list[i].id,
      groupsId: groupsId,
      quantity: _list[i].schedulingQuantity
    })
    _tagObj[groupsId].list.push(_list[i])
  }
  tagObj.value = _tagObj
  tagList.value = obj2arr(_tagObj)
}

// --------------------------- 任务下发 start ------------------------------
const handleSurplusRef = ref()
const taskLoading = ref(false)
const saveSurplusLoading = ref(false)
const saveTaskParams = ref({})

async function toSaveHandleSurplus() {
  try {
    saveSurplusLoading.value = true
    const _surplusRes = handleSurplusRef?.value.handleValidate()
    if (!_surplusRes) return
    saveTaskParams.value.matchingDetailList = _surplusRes
    await saveTask(saveTaskParams.value)
    ElNotification({
      title: '保存并下发成功',
      type: 'success',
      duration: 2500
    })
    surplusElementVisible.value = false
    emit('task-issue-success')
    handleClose()
  } catch (er) {
    console.log(er, '保存多余单元件处理')
  } finally {
    saveSurplusLoading.value = false
  }
}

async function toTaskIssue() {
  // if (props.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V && originElementSchedulingList.value.length > 0) {
  //   ElMessage.warning('智能线下存在未套料的单元件，请先进行套料！')
  //   return
  // }
  try {
    taskLoading.value = true
    saveTaskParams.value = {}
    let _box = []
    let _element = []
    let flag = true
    for (const item in tagObj.value) {
      // 显示的组才需要验证
      const _curList = tagObj.value[item]?.elementList
      if (showTagGroupIds.value.includes(Number(item)) && _curList?.length) {
        const { validResult, dealList } = tableValidate(_curList)
        if (validResult) {
          const copyDealList = deepClone(dealList)
          cleanUpData(copyDealList)
          const _list = copyDealList.map((v) => {
            return {
              askCompleteTime: v.askCompleteTime,
              boolStructuralEnum: v.boolStructuralEnum,
              groupsId: v.groupsId,
              productId: v.productId,
              projectId: v.projectId,
              quantity: v.needSchedulingQuantity,
              boolTypesettinglEnum: v.boolTypesettinglEnum,
              typesettingAssembleTypeEnum: v.typesettingAssembleTypeEnum,
              boolSectionSteel: v.boolSectionSteel,
              boolProcess: v.boolProcess
            }
          })
          _box = _box.concat(tagObj.value[item].ids)
          _element = _element.concat(_list)
        } else {
          curGroupsId.value = tagObj.value[item].groupsId
          flag = false
          break
        }
      } else {
        _box = _box.concat(tagObj.value[item].ids)
      }
      if (tagObj.value[item]?.unshowList && tagObj.value[item]?.unshowList?.length) {
        _element = _element.concat(tagObj.value[item]?.unshowList)
      }
    }
    if (!flag) {
      return
    }
    saveTaskParams.value = {
      boxSchedulingList: _box,
      elementDetailList: _element,
      productionLineTypeEnum: props.productionLineTypeEnum
    }
    console.log(saveTaskParams.value, 'saveTaskParams.value')
    if (surplusElementList.value.length && !surplusElementVisible.value) {
      surplusElementVisible.value = true
      return
    }
    await saveTask(saveTaskParams.value)
    ElNotification({
      title: '任务下发成功',
      type: 'success',
      duration: 2500
    })
    emit('task-issue-success')
    handleClose()
  } catch (error) {
    console.log('任务下发报错', error)
  } finally {
    taskLoading.value = false
  }
}

// --------------------------- 任务下发 end --------------------------------
</script>

<style scoped>
.tip {
  display: inline-block;
  color: #e6a23cc2;
  text-decoration: underline;
  margin-bottom: 10px;
}
</style>
