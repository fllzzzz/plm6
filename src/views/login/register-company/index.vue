<template>
  <div class="reg-company-container">
    <div class="reg-company-content">
      <el-input v-model="url" placeholder="请填写公司域名" class="input-with-select" @keyup.enter="submit">
        <template v-slot:prepend>
          <el-select v-model="protocol" placeholder="请选择">
            <el-option label="https://" value="https://" />
            <el-option label="http://" value="http://" />
          </el-select>
        </template>
        <template v-slot:append>
          <el-button style="color:#ffffff;" @click="submit">确认</el-button>
        </template>
      </el-input>
    </div>
  </div>
</template>

<script>
import { validRequestUrl } from '@/utils/validate'

export default {
  name: 'RegCompany',
  data() {
    return {
      url: '',
      protocol: 'https://'
    }
  },
  mounted() {
    // 默认赋值当前域名
    this.protocol = window.location.protocol + '//'
    this.url = window.location.host
  },
  methods: {
    async submit() {
      // TODO 向钢构会 校验是否存在
      const fullUrl = this.protocol + this.url
      if (validRequestUrl(fullUrl)) {
        await this.$store.dispatch('user/setRequestUrl', fullUrl)
        this.$emit('reg-result', true)
      } else {
        this.$message({ message: '错误格式的域名', type: 'warning' })
      }
    }
  }
}
</script>

<style lang="scss" scoped>
$bg: transparent;
.reg-company-container {
  position: fixed;
  height: 100%;
  width: 100%;
  background-color: $bg;
  .reg-company-content {
    z-index: 5;
    position: absolute;
    left: 0;
    right: 0;
    width: 520px;
    padding: 35px 35px 15px 35px;
    margin: 200px auto;
  }
}
</style>
<style lang="scss">
.reg-company-container {
  .el-select {
    .el-input {
      width: 100px;
    }
  }
  .input-with-select {
    .el-input-group__prepend {
      background-color: #fff;
    }
  }
  .el-input-group__append,
  .el-input-group__prepend {
    background-color: #4079d0;
  }

  .el-input__inner:focus {
    outline: none;
    border-color: #f5f7fa;
  }
}
</style>
