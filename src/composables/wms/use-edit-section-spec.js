import { nextTick, ref, watch } from 'vue'
import useMaxHeight from '@compos/use-max-height'

export default function useEditSectionSpec() {
  const specRef = ref()
  const drawerRef = ref()
  const editList = ref([]) // 编辑列表
  const editRow = ref({})
  const materialSelectVisible = ref(false) // 显示物料选择

  // 物料选择组件高度
  const { maxHeight: specSelectMaxHeight } = useMaxHeight(
    {
      mainBox: '.material-spec-select',
      extraBox: '.el-drawer__header',
      wrapperBox: ['.el-drawer__body'],
      navbar: false,
      clientHRepMainH: true,
      minHeight: 300
    },
    () => drawerRef.value.loaded
  )

  // 监听materialSelectVisible
  watch(
    materialSelectVisible,
    (val) => {
      if (val) {
        nextTick(() => {
          specRef.value.initSelected([editRow.value.sn])
        })
      }
    })

  function handleClickEditSpec(row) {
    editRow.value = row
    editList.value = [row]
    materialSelectVisible.value = true
  }

  function handleSpecChange(list) {
    const row = list[0]
    editRow.value.sn = row.sn
    editRow.value.specificationLabels = row.specificationLabels // 规格中文
    editRow.value.serialNumber = row.serialNumber // 科目编号 - 规格
    editRow.value.classifyId = row.classify.id // 科目id
    editRow.value.classifyFullName = row.classify.fullName // 全路径名称
    editRow.value.classifyName = row.classify.name // 当前科目名称
    editRow.value.classifyParentFullName = row.classify.parentFullName // 父级路径名称
    editRow.value.basicClass = row.classify.basicClass // 基础类型
    editRow.value.specification = row.spec // 规格
    editRow.value.specificationMap = row.specKV // 规格KV格式
    editRow.value.measureUnit = row.classify.measureUnit // 计量单位
    editRow.value.accountingUnit = row.classify.accountingUnit // 核算单位
    editRow.value.measurePrecision = row.classify.measurePrecision // 计量精度
    editRow.value.accountingPrecision = row.classify.accountingPrecision // 核算精度
    editRow.value.unitWeight = row.unitWeight // 单位重量
  }

  return {
    specSelectMaxHeight,
    specRef,
    drawerRef,
    editRow,
    editList,
    materialSelectVisible,
    handleClickEditSpec,
    handleSpecChange
  }
}
