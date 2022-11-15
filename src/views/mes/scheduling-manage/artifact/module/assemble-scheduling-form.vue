<template>
  <common-drawer ref="drawerRef" title="部件排产" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="100%">
    <template #titleRight>
      <common-button size="mini" type="success" @click="handleClose"> 上一步【构件排产预览】 </common-button>
      <common-button size="mini" :loading="taskLoading" type="primary" @click="toTaskIssue"> 任务下发 </common-button>
    </template>
    <template #content>
      <div class="head-container">
        <!-- <span class="filter-item">构件排产信息：</span> -->
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
        <span> 系统自动默认与构件相同的产线进行部件生产，也可在生产组列修改产线或生产组。</span>
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
            <el-tag :type="row.attributeType === '部件' ? 'warning' : 'success'">{{ row.attributeType }}</el-tag>
          </template>
        </el-table-column>
        <el-table-column prop="typesettingAssembleTypeEnum" :show-overflow-tooltip="true" label="部件类型" min-width="100" align="center">
          <template #default="{ row: { sourceRow: row } }">
            <span>{{row.typesettingAssembleTypeEnum?typesettingAssembleTypeEnum.VL[row.typesettingAssembleTypeEnum]:'-'}}</span>
          </template>
        </el-table-column>
        <el-table-column prop="serialNumber" :show-overflow-tooltip="true" label="编号" min-width="100" align="center">
          <template #default="{ row }">
            <span>{{ row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="specification" :show-overflow-tooltip="true" label="规格" min-width="120" align="center" />
        <el-table-column prop="length" :show-overflow-tooltip="true" label="长度（mm）" min-width="90" align="center" />
        <el-table-column prop="netWeight" :show-overflow-tooltip="true" label="单净重（kg）" min-width="90" align="center" />
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
              :options="classIdGroupsObj[row.assembleConfigId]?.list"
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
      <el-divider v-if="otherData.length"><span class="title">型材</span></el-divider>
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
            <el-tag :type="row.attributeType === '部件' ? 'warning' : 'success'">{{ row.attributeType }}</el-tag>
          </template>
        </el-table-column>
        <el-table-column prop="typesettingAssembleTypeEnum" :show-overflow-tooltip="true" label="部件类型" min-width="100" align="center">
          <template #default="{ row: { sourceRow: row } }">
            <span>{{row.typesettingAssembleTypeEnum?typesettingAssembleTypeEnum.VL[row.typesettingAssembleTypeEnum]:'-'}}</span>
          </template>
        </el-table-column>
        <el-table-column prop="serialNumber" :show-overflow-tooltip="true" label="编号" min-width="100" align="center" />
        <el-table-column prop="specification" :show-overflow-tooltip="true" label="规格" min-width="120" align="center" />
        <el-table-column prop="length" :show-overflow-tooltip="true" label="长度（mm）" min-width="90" align="center" />
        <el-table-column prop="netWeight" :show-overflow-tooltip="true" label="单净重（kg）" min-width="90" align="center" />
        <el-table-column prop="quantity" :show-overflow-tooltip="true" label="数量" min-width="90" align="center" />
      </common-table>
      <handle-surplus-assemble-dialog
        ref="handleSurplusRef"
        v-model:visible="surplusAssembleVisible"
        :surplusList="surplusAssembleList"
        :groupsId="props.artifactList[0]?.groups?.id"
      >
        <template #saveBtn>
          <common-button size="mini" :loading="saveSurplusLoading" type="primary" @click="toSaveHandleSurplus"> 保存并下发 </common-button>
        </template>
      </handle-surplus-assemble-dialog>
    </template>
  </common-drawer>
</template>

<script setup>
import { getAssemble } from '@/api/mes/scheduling-manage/artifact'
import { saveTask } from '@/api/mes/scheduling-manage/common'
import { defineProps, defineEmits, ref, inject, reactive, computed } from 'vue'
import moment from 'moment'
import { ElNotification } from 'element-plus'

import { artifactProductLineEnum, mesBuildingTypeSettingAssembleTypeEnum } from '@enum-ms/mes'
import { componentTypeEnum } from '@enum-ms/mes'
import { isBlank, deepClone } from '@/utils/data-type'
import { obj2arr } from '@/utils/convert/type'

import useTableValidate from '@compos/form/use-table-validate'
import { manualFetchGroupsTree } from '@compos/mes/scheduling/use-scheduling-groups'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import tagTabs from '@comp-common/tag-tabs'
import handleSurplusAssembleDialog from './handle-surplus-assemble-dialog'

const productType = componentTypeEnum.ASSEMBLE.V

const drawerRef = ref()
const emit = defineEmits(['update:visible', 'task-issue-success'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  artifactList: {
    type: Array,
    default: () => []
  },
  productionLineTypeEnum: {
    type: [Number, String]
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook })

const factoryIds = inject('curFactoryIds')
const classIdGroupsObj = reactive({}) // {部件类型id：{list：groupsTree，obj：groupsObj}}

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

const paGroupId = -1 // 母件 默认一组 组id为-1
const tableLoading = ref()
const tagObj = ref({})
const tagList = ref([])
const showTagGroupIds = ref([])
const curGroupsId = ref()
const originAssembleSchedulingList = ref([])
const surplusAssembleList = ref([])
const surplusAssembleVisible = ref(false)

const tableData = computed(() => tagObj.value[curGroupsId.value]?.assembleList || [])
const otherData = computed(() => tagObj.value[curGroupsId.value]?.otherList || [])
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
  initArtifactData(props.artifactList)
  fetch()
}

async function fetch() {
  // const ids = (curGroupsId.value && tagObj.value[curGroupsId.value]?.ids) || []
  // if (!ids || !ids.length) return
  try {
    tableLoading.value = true
    surplusAssembleList.value = []
    const _ids = props.artifactList.map((v) => {
      return {
        id: v.id,
        quantity: v.schedulingQuantity
      }
    })
    const { assembleSchedulingList, assembleTypesetting, surplusAssemble } = await getAssemble(_ids)
    showTagGroupIds.value = []
    originAssembleSchedulingList.value = assembleSchedulingList
    // 处理部件信息
    for (let i = 0; i < assembleSchedulingList.length; i++) {
      const v = assembleSchedulingList[i]
      if (v?.groupsId && tagObj.value[v.groupsId]) {
        showTagGroupIds.value.push(v.groupsId)
        tagObj.value[v.groupsId].assembleList = []
        tagObj.value[v.groupsId].unshowList = []
        tagObj.value[v.groupsId].otherList = []
        for (let o = 0; o < v.assembleList.length; o++) {
          const _o = v.assembleList[o]
          _o.boolStructuralEnum = false
          _o.attributeType = '部件'
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
            tagObj.value[v.groupsId].assembleList.push({ ..._o })
          }

          if (isBlank(classIdGroupsObj[_o.assembleConfigId])) {
            classIdGroupsObj[_o.assembleConfigId] = await manualFetchGroupsTree({
              productType,
              structureClassId: _o.assembleConfigId,
              _factoryIds: factoryIds.value
            })
          }
        }
      }
    }
    // 处理母件信息
    if (assembleTypesetting?.length) {
      const _list = []
      const _unshowList = []
      const _otherList = []
      for (let x = 0; x < assembleTypesetting.length; x++) {
        const v = assembleTypesetting[x]
        v.productId = v.id
        v.attributeType = '套料'
        v.boolStructuralEnum = true
        v.boolTypesettinglEnum = true
        v.needSchedulingQuantity = 1
        if (x !== 0) {
          v.groupsId = '同上'
          v.askCompleteTime = '同上'
        }
        if (v.assembleConfigId && isBlank(classIdGroupsObj[v.assembleConfigId])) {
          classIdGroupsObj[v.assembleConfigId] = await manualFetchGroupsTree({
            productType,
            structureClassId: v.assembleConfigId,
            _factoryIds: factoryIds.value
          })
        }
        // _list.push(v)
        if (
          (props.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V &&
            v.typesettingAssembleTypeEnum === mesBuildingTypeSettingAssembleTypeEnum.FINISHED.V) ||
          (props.productionLineTypeEnum === artifactProductLineEnum.TRADITION.V && !v.boolProcess)
        ) {
          _unshowList.push({
            boolStructuralEnum: v.boolStructuralEnum,
            boolTypesettinglEnum: v.boolTypesettinglEnum,
            typesettingAssembleTypeEnum: v.typesettingAssembleTypeEnum,
            boolProcess: v.boolProcess,
            productId: v.productId,
            projectId: v.projectId,
            quantity: v.quantity
          })
          _otherList.push(v)
        } else {
          _list.push(v)
        }
      }
      const _obj = {
        label: '母件',
        mergeQuantity: 0,
        mergeWeight: 0,
        assembleList: _list,
        unshowList: _unshowList,
        otherList: _otherList,
        ids: [],
        groupsId: paGroupId
      }
      tagObj.value[paGroupId] = _obj
      tagList.value.push(_obj)
      showTagGroupIds.value.push(paGroupId)
    }
    if (props.productionLineTypeEnum & artifactProductLineEnum.INTELLECT.V) {
      surplusAssembleList.value = surplusAssemble || []
    }
    curGroupsId.value = showTagGroupIds.value[0]
  } catch (error) {
    console.log('获取部件排产列表失败', error)
  } finally {
    tableLoading.value = false
  }
}

// 构件按生产组分类
function initArtifactData(list) {
  const _list = list
  const _tagObj = {} // 构件信息对象{groupsId:{}}
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
    surplusAssembleVisible.value = false
    emit('task-issue-success')
    handleClose()
  } catch (er) {
    console.log(er, '保存多余部件处理')
  } finally {
    saveSurplusLoading.value = false
  }
}

async function toTaskIssue() {
  // if (props.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V && originAssembleSchedulingList.value.length > 0) {
  //   ElMessage.warning('智能线下存在未套料的部件，请先进行套料！')
  //   return
  // }
  try {
    taskLoading.value = true
    saveTaskParams.value = {}
    let _artifact = []
    let _assemble = []
    let flag = true
    for (const item in tagObj.value) {
      // 显示的组才需要验证
      const _curList = tagObj.value[item]?.assembleList
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
          _artifact = _artifact.concat(tagObj.value[item].ids)
          _assemble = _assemble.concat(_list)
        } else {
          curGroupsId.value = tagObj.value[item].groupsId
          flag = false
          break
        }
      } else {
        _artifact = _artifact.concat(tagObj.value[item].ids)
      }
      if (tagObj.value[item]?.unshowList && tagObj.value[item]?.unshowList?.length) {
        _assemble = _assemble.concat(tagObj.value[item]?.unshowList)
      }
    }
    if (!flag) {
      return
    }
    saveTaskParams.value = {
      artifactSchedulingList: _artifact,
      assembleDetailList: _assemble,
      productionLineTypeEnum: props.productionLineTypeEnum
    }
    console.log(saveTaskParams.value, 'saveTaskParams.value')
    if (surplusAssembleList.value.length && !surplusAssembleVisible.value) {
      surplusAssembleVisible.value = true
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
