<template>
  <el-drawer
    :title="crud.status.title"
    :visible="crud.status.cu > 0"
    :before-close="crud.cancelCU"
    direction="rtl"
    size="50%"
  >
    <template slot="title">
      <span>{{ crud.status.title }}</span>
      <el-button :loading="crud.status.cu === 2" type="success" size="mini" @click.stop="crud.submitCU">提交</el-button>
    </template>
    <div class="el-drawer-container">
      <el-form ref="form" :model="form" :rules="rules" size="small" label-width="150px">
        <div id="baseInfo">
          <el-form-item label="公司名称" prop="name">
            <el-input
              v-model="form.name"
              type="text"
              placeholder="请填写名称"
              class="input-underline"
              style="width: 270px;"
            />
          </el-form-item>
          <el-form-item label="社会统一信用代码" prop="socialCode">
            <el-input
              v-model="form.socialCode"
              type="text"
              placeholder="请填写社会统一信用代码"
              class="input-underline"
              style="width: 270px;"
            />
          </el-form-item>
          <el-form-item label="母公司" prop="isParent">
            <template slot="label">
              <el-tooltip
                effect="light"
                content="只可选择一个公司为母公司，若选择新的母公司，原母公司会被取消"
                placement="left"
              >
                <div style="display:inline-block;">
                  <span>母公司</span>
                  <i class="el-icon-info" style="color:#909399" />
                </div>
              </el-tooltip>
            </template>
            <el-checkbox v-model="form.isParent" :true-label="enabledEnum.TRUE.V" :false-label="enabledEnum.FALSE.V" />
          </el-form-item>
          <el-form-item label="状态" prop="enabled">
            <common-radio
              :value.sync="form.enabled"
              :options="enabledEnum"
              type="enum"
            />
          </el-form-item>
          <el-form-item label="排序" prop="sort">
            <el-input-number
              v-model.number="form.sort"
              :min="1"
              :max="999"
              :step="1"
              controls-position="right"
              style="width: 270px;"
            />
          </el-form-item>
          <el-form-item label="备注" prop="remark">
            <el-input
              v-model="form.remark"
              type="textarea"
              :autosize="{ minRows: 6, maxRows: 8}"
              placeholder="请填写备注"
              style="max-width: 500px;"
            />
          </el-form-item>
        </div>
        <!-- <hr class="gradient-line"> -->
        <el-divider><span class="title">银行账号</span></el-divider>
        <el-table
          ref="table"
          :data="form.bankAccounts"
          :cell-class-name="handelCellClassName"
          style="width: 100%"
          :border="$TBS.BORDER"
        >
          <el-table-column v-if="$TBS.INDEX" :label="$TBS.INDEX_LABEL ? '序号': ''" type="index" align="center" width="60" />
          <el-table-column prop="depositBank" label="开户行" align="left" min-width="160">
            <template v-slot="scope">
              <el-input
                v-model="scope.row.depositBank"
                size="small"
                placeholder="开户行"
              />
            </template>
          </el-table-column>
          <el-table-column prop="account" label="账号" align="left" min-width="160">
            <template v-slot="scope">
              <el-input
                v-model="scope.row.account"
                size="small"
                placeholder="银行账号"
                maxlength="30"
              />
            </template>
          </el-table-column>
          <el-table-column label="操作" align="center">
            <template v-slot="scope">
              <el-button size="small" class="el-icon-delete" type="danger" @click="deleteRow(scope.$index)" />
            </template>
          </el-table-column>
        </el-table>
        <div class="add-row-box">
          <el-button size="mini" icon="el-icon-circle-plus-outline" type="warning" style="margin-right:15px" @click="addRow()">继续添加</el-button>
          <el-tooltip
            effect="light"
            :content="`银行账号（不必填）添加规则：\n
          1、开户行：必填，1 - 50 个字符；\n
          2、账户：必填，1 - 30 个字符。\n`"
            placement="right"
          >
            <div style="display:inline-block;">
              <el-tag type="info">银行账号添加规则</el-tag>
            </div>
          </el-tooltip>
        </div>
      </el-form>
    </div>
  </el-drawer>
</template>

<script>
import CRUD, { form } from '@crud/crud'
import { tableValidate } from '@/utils/other'
import { enabledEnum } from '@/utils/enum/index'
import { validatorEnOrNum, validatorNatural } from '@/utils/validatePattern'

const defaultForm = {
  id: undefined,
  name: '',
  socialCode: '',
  isParent: false,
  enabled: enabledEnum.TRUE.V,
  sort: 1,
  remark: '',
  bankAccounts: []
}

export default {
  mixins: [form(defaultForm)],
  data() {
    return {
      enabledEnum,
      rules: {
        name: [
          { required: true, message: '请填写公司名称', trigger: 'blur' },
          { min: 1, max: 60, message: '长度在 1 到 60 个字符', trigger: 'blur' }
        ],
        socialCode: [
          // { required: true, message: '请填写社会统一信用代码', trigger: 'blur' },
          { min: 1, max: 30, message: '长度在 1 到 30 个字符', trigger: 'blur' },
          { pattern: validatorEnOrNum.pattern, message: validatorEnOrNum.message, trigger: 'blur' }
        ],
        sort: [
          { required: true, message: '请填写排序值', trigger: 'blur', type: 'number' }
        ],
        remark: [{ max: 500, message: '不能超过500个字符', trigger: 'blur' }]
      },
      tableRules: {
        depositBank: [{ required: true, max: 50, message: '不能超过 50 个字符', trigger: 'blur' }],
        account: [
          { max: 30, message: '长度不超过 30 个字符', trigger: 'blur' },
          { pattern: validatorNatural, message: '请输入数字', trigger: 'blur' }
        ]
      }
    }
  },
  methods: {
    [CRUD.HOOK.afterToAdd]() {
      this.addRow()
    },
    [CRUD.HOOK.beforeSubmit]() {
      return this.validateTable()
    },
    validateTable() {
      let flag = true
      const list = this.form.bankAccounts
      let message = '请填写数据'
      if (list && list.length > 0) {
        // TODO: 考虑封装
        list.forEach(row => {
          for (const rule in this.tableRules) {
            row.verify = row.verify ? row.verify : {}
            row.verify[rule] = tableValidate(this.tableRules[rule], row[rule])
            if (!row.verify[rule]) {
              flag = false
            }
          }
        })
        if (!flag) {
          this.form.bankAccounts = Object.assign([], list)
          message = '请修正表格中标红的信息, 填写限制请参照标题旁边的“信息符号(i)”'
        }
      }
      if (!flag) {
        this.$message({ message, type: 'error' })
      }
      return flag
    },
    // 添加行
    addRow: function() {
      this.form.bankAccounts.push({ verify: {}})
    },
    // 删除行
    deleteRow: function(index) {
      this.form.bankAccounts.splice(index, 1)
    },
    handelCellClassName({ row, column, rowIndex, columnIndex }) {
      let flag = true
      if (row.verify && Object.keys(row.verify) && Object.keys(row.verify).length > 0) {
        if (row.verify[column.property] === false) {
          flag = tableValidate(this.tableRules[column.property], row[column.property])
        }
        if (flag) {
          row.verify[column.property] = true
        }
      }
      return flag ? '' : 'mask-td'
    }
  }
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
>>>.el-input-number .el-input__inner {
  text-align: left;
}
>>>.el-dialog__body{
  padding: 10px 20px;
}
>>>.el-drawer__header {
    margin-bottom: 10px;
}
.el-drawer-container {
  padding: 0 20px 20px 20px;
  overflow: auto;
}
>>>.input-underline {
  // width: calc((95vw - 40px)/3);
  width: 200px;
  margin-right: 0;
  input{
    border-top:0;
    border-left:0;
    border-right:0;
    border-radius: 0;
  }
}
.add-row-box {
  display: flex;
  flex-direction: row;
  justify-content: center;
  align-items: center;
  margin-top: 20px;
}
</style>
